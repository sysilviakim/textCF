source(here::here("R/utilities.R"))
source(here::here("R/classifier_utilities.R"))
# Subclass dataset function to hold images for analysis

# Partly copied from https://blogs.rstudio.com/ai/posts/2020-11-30-torch-brain-segmentation/
candidate_image_dataset <- torch::dataset(
  name = "campaign_image_dataset",
  sample_weights = NULL,
  angle = NULL,
  flip_thresh = NULL,
  rescale_thresh = NULL,
  images = NULL,
  targets = NULL,
  initialize = function(img_dir, sample_weights = NULL,
                        transform_params = NULL,
                        class_labels = CLASS_NAMES) {
    stopifnot(
      dir.exists(img_dir),
      all(sample_weights > 0),
      is.null(transform_params) || all(names(transform_params) %in% c("angle", "flip", "rescale"))
    ) # R thinks NULL >0, bizarrely
    self$root <- img_dir
    self$labels <- class_labels

    # Store images as simple vector of file paths, with names designating true label
    # TODO names not retained correctly
    images <- file.path(img_dir, self$labels) %>%
      lapply(list.files, full.names = TRUE)
    new_names <- mapply(rep, self$labels, each = lengths(images)) %>%
      unlist(use.names = FALSE)
    images <- unlist(images, use.names = FALSE)
    names(images) <- new_names
    self$images <- images
    self$targets <- match(names(images), setNames(self$labels, self$labels)) %>%
      torch::torch_tensor() %>%
      torch_squeeze()

    stopifnot(is.null(sample_weights) || length(sample_weights) == length(CLASS_NAMES))
    if (!is.null(sample_weights)) {
      self$sample_weights <- self$compute_sample_weights(self$images, sample_weights)
    }
    if (!is.null(transform_params)) {
      with(transform_params, {
        self$angle <- angle %% 360
        self$flip_thresh <- flip
        self$rescale_thresh <- rescale
      })
    }
  },
  .getitem = function(i) {
    if (!is.null(self$sample_weights)) {
      i <- sample(length(self), 1, prob = self$sample_weights)
    }
    # True class label
    y <- torch_tensor(match(names(self$images[i]), self$labels)) %>%
      torch_squeeze()
    img <- self$images[i] %>%
      read_image(to_raster = FALSE, drop_alpha = TRUE) %>%
      torchvision::transform_to_tensor() %>%
      (function(x) x$to(device = DEVICE)) %>%
      torchvision::transform_resize(c(224, 224))
    # Found PNG with no channel dimension - bizarre
    if (dim(img)[[1]] != 3) {
      img <- img$add(torch_zeros(c(3, dim(img)[2:3])))
    }
    img <-
      do.call(torchvision::transform_normalize, c(img, RESNET18_CONSTANTS))

    if (!is.null(self$angle)) {
      img <- self$rotate(img, self$angle)
    }
    if (!is.null(self$flip_thresh)) {
      img <- self$flip(img, self$flip_thresh)
    }
    if (!is.null(self$rescale_thresh)) {
      img <- self$rescale(img, self$rescale_thresh)
    }
    list(x = img, y = y)
  },
  # Sampling weights to apply to each class. Unweighted, all observations have an equal chance of being
  # sampled; weights adjust the probabilities
  compute_sample_weights = function(images, weights) {
    counts <- table(names(images))
    proportions <- counts / length(self)
    proportions <- proportions[names(weights)] * weights
    proportions[names(images)] * (1 / counts)[names(images)]
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
  rescale = function(img, scale) {
    img_size <- dim(img)[2]
    rnd_scale <- runif(1, 1 - scale, 1 + scale)
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
  .length = function() length(self$images)
)
