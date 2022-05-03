source(here::here("R", "utilities.R"))
source(here::here("R", "image", "classifier_utilities.R"))
source(here::here("R", "dropbox_utilities.R"))
# Subclass dataset function to hold images for analysis

# Partly copied from https://blogs.rstudio.com/ai/posts/2020-11-30-torch-brain-segmentation/
candidate_image_dataset <- torch::dataset(
  name = "candidate_image_dataset",
  sample_weights = NULL,
  angle = NULL,
  flip_thresh = NULL,
  rescale_bounds = NULL,
  images = NULL,
  targets = NULL,
  image_types = NULL,
  uses_dropbox = NULL,
  uses_sample = FALSE,
  uses_transform = FALSE,
  full_image_paths = TRUE,
  print = function() {
    class_count <- table(names(self$images))
    cat(
      "A candidate image dataset containing", length(self), "images",
      "in", length(class_count), "classes", "\n"
    )
    print(class_count)
    cat("Directory", self$root, "\n")
  },
  initialize = function(img_dir,
                        sample_weights = NULL,
                        transform_params = NULL,
                        class_labels = CLASS_NAMES,
                        use_dropbox = FALSE,
                        dropbox_token = NULL,
                        full_image_paths = TRUE,
                        debug = FALSE) {
    stopifnot(
      all(sample_weights > 0),
      is.null(transform_params) || all(names(transform_params) %in% c("angle", "flip", "rescale_min", "rescale_max") &&
        length(transform_params) == 4)
    ) # R thinks NULL >0, bizarrely

    if (use_dropbox && is.null(dropbox_token)) {
      stop("dropbox_token is NULL, but use_dropbox = TRUE")
    }

    private$.prev_error <- getOption("error")
    if (debug) {
      options(error = debug_handler)
      message("Debugging enabled for this instance")
    }
    self$root <- img_dir
    self$labels <- class_labels
    self$uses_dropbox <- use_dropbox
    self$full_image_paths <- full_image_paths
    if (self$full_image_paths) {
      private$.path_transform <- identity
    } else {
      private$.path_transform <- basename
    }

    # Store images as simple vector of file paths, with names designating true label
    images <- file.path(img_dir, self$labels)

    # Whether using local storage or Dropbox, list images in train, test, and valid directories
    if (self$uses_dropbox) {
      self$configure_dropbox(dropbox_token)
      images <- lapply(images,
        dropbox_list_files,
        dropbox_token = private$.dropbox_token,
        recursive = TRUE,
        full_paths = TRUE, target = "files", pattern = ".*"
      )
    } else {
      images <- lapply(images, list.files, full.names = TRUE)
    }
    new_names <- mapply(rep, self$labels, each = lengths(images)) %>%
      unlist(use.names = FALSE)
    images <- unlist(images, use.names = FALSE)
    names(images) <- new_names
    self$images <- images

    # If downloading from Dropbox, we need to know
    # each image's file type to read it into memory
    self$image_types <- c(
      jpg = "jpeg",
      jpeg = "jpeg",
      png = "png"
    )[tolower(tools::file_ext(self$images))] %>%
      unname()
    if (use_dropbox && any(is.na(self$image_types))) stop("Some images have unknown types, and use_dropbox = TRUE")

    # Correct labels
    self$targets <- match(
      names(images),
      setNames(
        self$labels,
        self$labels
      )
    ) %>%
      torch::torch_tensor() %>%
      torch_squeeze()

    stopifnot(is.null(sample_weights) ||
      length(sample_weights) == length(CLASS_NAMES))
    if (!is.null(sample_weights)) {
      self$sample_weights <- self$compute_sample_weights(self$images, sample_weights)
      self$uses_sample <- TRUE
    }
    # Must specify all params or none
    if (!is.null(transform_params)) {
      with(transform_params, {
        self$angle <- angle %% 360
        self$flip_thresh <- flip
        self$rescale_bounds <- c(
          rescale_min = rescale_min,
          rescale_max = rescale_max
        )
        self$uses_transform <- TRUE
      })
    }
  },
  .getitem = function(i) {
    if (self$uses_sample) {
      i <- sample(length(self), 1, prob = self$sample_weights)
    }
    path <- self$images[i]
    # True class label
    y <- torch_tensor(match(names(path), self$labels)) %>%
      torch_squeeze()
    # TODO vectorize image downloading for length(i) > 1
    if (self$uses_dropbox) {
      if (!private$.dropbox_token$can_refresh()) {
        private$.dropbox_token <- dropbox_authenticate(refresh = TRUE)
      } else {
        private$.dropbox_token$refresh()
      }
      img <- dropbox_download_file(
        dropbox_token = private$.dropbox_token,
        dropbox_path = path,
        save_file = FALSE,
        content_parser = read_image_memory,
        image_type = self$image_types[i],
        to_raster = FALSE,
        drop_alpha = TRUE
      )
    } else {
      img <- read_image(path,
        to_raster = FALSE,
        drop_alpha = TRUE
      )
    }
    img <- img %>%
      torchvision::transform_to_tensor() %>%
      (function(x) x$to(device = DEVICE)) %>%
      # if(!all(dim(img)[-1] == c(224, 224)))
      torchvision::transform_resize(c(224, 224))
    # Found PNG with no channel dimension - bizarre
    if (dim(img)[[1]] != 3) {
      img <- img$add(torch_zeros(c(3, dim(img)[2:3])))
    }
    img <-
      do.call(torchvision::transform_normalize, c(img, RESNET18_CONSTANTS))
    if (self$uses_transform) {
      img <- self$rotate(img, self$angle) %>%
        self$flip(self$flip_thresh) %>%
        self$rescale(self$rescale_bounds)
    }
    list(x = img, y = y, path = private$.path_transform(path))
  },
  # Sampling weights to apply to each class. Unweighted, all observations have an equal chance of being
  # sampled. For instance, weights of c(1, 20) increase by a factor of 20 the second class's chance of being sampled
  compute_sample_weights = function(images, weights) {
    counts <- table(names(images)) # names are true labels
    proportions <- counts / length(self)
    proportions <- proportions[names(weights)] * weights
    proportions[names(images)]
  },
  rotate = function(img, angle, ...) {
    angle <- runif(1, 1 - angle, 1 + angle)
    torchvision::transform_rotate(img, angle = angle, ...)
  },
  flip = function(img, flip) {
    if (runif(1) > flip) {
      img <- torchvision::transform_hflip(img)
    }
    img
  },
  rescale = function(img, rescale_bounds) {
    img_size <- dim(img)[2]
    rnd_scale <- runif(1, rescale_bounds[[1]], rescale_bounds[[2]])
    img <- torchvision::transform_resize(img, size = rnd_scale * img_size)
    diff <- dim(img)[2] - img_size
    if (diff > 0) {
      top <- ceiling(diff / 2)
      left <- ceiling(diff / 2)
      img <- torchvision::transform_crop(img, top, left, img_size, img_size)
    } else if (diff < 0) {
      img <- torchvision::transform_pad(img,
        padding = -c(
          ceiling(diff / 2),
          floor(diff / 2),
          ceiling(diff / 2),
          floor(diff / 2)
        )
      )
    }
    img
  },
  .length = function() length(self$images),
  # If debugging enabled, reset previous error handler
  configure_dropbox = function(dropbox_token) {
    dropbox_token <- dropbox_token_get(dropbox_token)
    if (!dropbox_token$can_refresh()) dropbox_token <- dropbox_authenticate(refresh = TRUE)
    dropbox_token$refresh()
    private$.dropbox_token <- dropbox_token
    self
  },
  finalize = function() options(error = private$.prev_error),
  private = list(
    .prev_error = NULL,
    .dropbox_token = NULL,
    .path_transform = NULL
  )
)
