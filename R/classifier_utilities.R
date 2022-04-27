library(torch)
library(torchdatasets)
library(torchvision)

# Much of this copied from R torch official site
# (https://blogs.rstudio.com/ai/posts/2020-10-19-torch-image-classification/)

# Expected by pretrained classifier

RESNET18_CONSTANTS <-
  list(mean = c(0.485, 0.456, 0.406), std = c(0.229, 0.224, 0.225))
DEVICE <- if (cuda_is_available()) torch_device("cuda:0") else "cpu"
CLASS_NAMES <- c("No Trump" = "no_trump", "Trump" = "trump")
# Appropriate functions to read different image file types
IMAGE_FUNS <- list(
  "jpg" = jpeg::readJPEG,
  "jpeg" = jpeg::readJPEG,
  "png" = png::readPNG
)
CNN_STATISTICS <- data.frame(
  actual_class = integer(),
  pred_class = integer(),
  image = character(),
  p_trump = double(),
  log_p_trump = double(),
  batch_loss = double()
)


transform_image <- function(img, train = FALSE, dims = c(224, 224)) {
  img <- img[, , -4]
  # first convert image to tensor
  img <- img %>%
    transform_to_tensor() %>%
    (function(x) x$to(device = DEVICE)) %>%
    transform_resize(dims)
  # then move to the GPU (if available)
  if (train) {
    # Remove alpha channel (PNGs are RGBA)
    img <- img %>%
      transform_random_resized_crop(size = c(224, 224)) %>%
      # data augmentation
      transform_color_jitter() %>%
      # data augmentation
      transform_random_horizontal_flip()
  } else {
    img <- img %>%
      transform_resize(256) %>%
      transform_center_crop(224)
  }
  # normalize according to what is expected by resnet
  do.call(transform_normalize, c(img, RESNET18_CONSTANTS))
}

# Adapted from
# https://stackoverflow.com/questions/23861000/displaying-images-in-r-in-version-3-1-0
render_image <- function(path, pause = FALSE) {
  if ((ext <- tools::file_ext(path)) %in% c("jpg", "jpeg")) {
    image <- jpeg::readJPEG(path, native = TRUE)
    plot(0:1, 0:1, type = "n", ann = FALSE, axes = FALSE)
  } else if (ext == "png") {
    image <- png::readPNG(path)
    plot.new()
  } else {
    stop("Don't know how to handle extension", ext)
  }
  rasterImage(image, 0, 0, 1, 1)
  if (pause) Sys.sleep(3)
}

# Pad matrix along shorter dimension with fill init_value
# Quick way of making square images for plotting; unsure if correct
pad_matrix <- function(mat, fill = "#FFFFFF") {
  stopifnot("Fill must match type of matrix" = typeof(mat) == typeof(fill))
  dims <- dim(mat)
  if (length(dims) < 2) stop("Matrix must have 2 or more dimensions")
  difference <- dims[[1]] - dims[[2]]
  if (difference > 0) {
    # More rows than columns
    out <- cbind(mat, matrix(fill, nrow = dims[[1]], ncol = difference))
  } else if (difference < 0) {
    # More columns than rows
    out <- rbind(mat, matrix(fill, nrow = -difference, ncol = dims[[2]]))
  } else {
    out <- mat
  }
  as.raster(out)
}

# Load image from a file path
read_image <- function(path, to_raster = TRUE, drop_alpha = TRUE) {
  fun <- switch(tools::file_ext(path),
    "jpg" = ,
    "jpeg" = jpeg::readJPEG,
    "png" = png::readPNG,
    {
      stop("Unknown image format")
    }
  )
  img <- fun(path)
  preprocess_image(img, to_raster = to_raster, drop_alpha = drop_alpha)
}
# Read image from character array instead of a file
read_image_memory <- function(obj, image_type = c("jpeg", "png"),
                              to_raster = TRUE,
                              drop_alpha = TRUE) {
  image_type <- match.arg(image_type)
  if (image_type == "jpeg") {
    img <- jpeg::readJPEG(obj)
  } else {
    img <- png::readPNG(obj)
  }
  preprocess_image(img, to_raster = to_raster, drop_alpha = drop_alpha)
}

# Helper to convert image to raster and/or remove alpha channel
preprocess_image <- function(img, to_raster = TRUE, drop_alpha = TRUE) {
  if (to_raster) {
    img <- as.raster(img)
  }
  # Remove alpha (fourth) channel from png, unneeded for neural network
  if (drop_alpha && length(dim(img)) == 3) {
    img <- img[, , -4]
  }
  img
}

# Plots all images stored in a directory, chunked into plots of given dimension (default 4 x 4).
# Crops to square dimensions
# May cause memory error if given too many images
inspect_images_from_paths <- function(paths,
                                      labels = "",
                                      plot_dims = c(4, 4), shuffle = FALSE,
                                      pause = if (interactive()) 3 else 0) {
  if (length(paths) == 0) stop("Zero-length paths argument")
  if (!length(labels) %in% c(1, length(paths))) {
    stop(length(paths), " images, but ", length(labels), " labels")
  }
  size <- prod(plot_dims)
  if (all(file.info(paths)[["isdir"]])) paths <- list.files(paths, full.names = TRUE)
  images <- paths[tools::file_ext(paths) %in% names(IMAGE_FUNS)]
  if (shuffle) {
    images <- images[sample(length(images), length(images), replace = FALSE)]
  }
  n_plots <- ceiling(length(images) / size)
  old <- par(c("mar", "mfcol"))
  par(mfcol = plot_dims, mar = rep(1, 4))
  # splits <- rep(seq(n_plots), each = size)[seq_along(images)]
  splits <- split(
    seq_along(images),
    rep(seq_len(n_plots), each = size)[seq_along(images)]
  )

  # Split list into chunks, each with a distinct class
  images <- data.frame(name = basename(images), image = images, label = labels) %>%
    t()
  # images <- split(images, splits)
  #
  ## Recursively traverses possibly nested list of image rasters and plots them
  # plot_image_list <- function(lst, pause = 0) {
  # if (is.list(lst[[1]])) {
  # lapply(lst, plot_image_list, pause = pause)
  # } else {
  # for (i in seq_len(nrow(lst))) {
  ## browser()
  ## plot(pad_matrix(img))
  # plot(lst[i, "img"])
  # title(lst[i, "name"])
  # mtext(lst[i, "label"], side = 1)
  # }
  # }
  # Sys.sleep(pause)
  # }
  plot_image_params <- function(params, pause = 3) {
    for (i in seq_len(ncol(params))) {
      plot(read_image(params["image", i], drop_alpha = FALSE))
      title(params["name", i], cex.main = 0.85)
      mtext(params["label", i], side = 1)
    }
    Sys.sleep(pause)
  }

  # lapply(images, function(x) {
  # lapply(x[["image"]], read_image, drop_alpha = FALSE) %>%
  # plot_image_list(pause = pause)
  # }, images, labels)
  images <- lapply(splits, function(x) images[, x, drop = FALSE]) %>%
    lapply(plot_image_params, pause = 3)

  # Reset plot parameters
  on.exit(do.call(par, old))
}

# Copied from https://blogs.rstudio.com/ai/posts/2020-10-19-torch-image-classification/
find_lr <- function(train_dl, optimizer, init_value = 1e-8,
                    final_value = 10, beta = 0.98) {
  num <- train_dl$.length()
  mult <- (final_value / init_value)^(1 / num)
  lr <- init_value
  optimizer$param_groups[[1]]$lr <- lr
  avg_loss <- best_loss <- batch_num <- 0
  losses <- log_lrs <- c()

  coro::loop(for (b in train_dl) {
    batch_num <- batch_num + 1
    optimizer$zero_grad()
    output <- model(b[[1]]$to(device = DEVICE))
    loss <- criterion(output, b[[2]]$to(device = DEVICE))

    # Compute the smoothed loss
    avg_loss <- beta * avg_loss + (1 - beta) * loss$item()
    smoothed_loss <- avg_loss / (1 - beta^batch_num)
    # Stop if the loss is exploding
    if (batch_num > 1 && smoothed_loss > 4 * best_loss) break
    # Record the best loss
    if (smoothed_loss < best_loss || batch_num == 1) best_loss <- smoothed_loss

    # Store the values
    losses <- c(losses, smoothed_loss)
    log_lrs <- c(log_lrs, (log(lr, 10)))

    loss$backward()
    optimizer$step()

    # Update the lr for the next step
    lr <- lr * mult
    optimizer$param_groups[[1]]$lr <- lr
  })
  data.frame(log_lrs = log_lrs, losses = losses)
}

train_batch <- function(b, model, optimizer, criterion, scheduler) {
  optimizer$zero_grad()
  output <- model(b[[1]]$to(device = DEVICE))
  loss <- criterion(output, b[[2]]$to(device = DEVICE))
  loss$backward()
  optimizer$step()
  scheduler$step()
  loss$item()
}

valid_batch <- function(b, model, criterion) {
  output <- model(b[[1]])
  loss <- criterion(output, b[[2]]$to(device = DEVICE))
  loss$item()
}

test_batch <- function(b, model, criterion) {
  output <- model(b[["x"]])
  labels <- b[["y"]]$to(device = DEVICE)
  loss <- criterion(output, labels)
  predictions <- output$data()$clone()$detach()
  # torch_max returns a list with position 1 containing the values (here log predicted probabilities)
  # and position 2 containing the respective indices
  results <- data.frame(
    actual_class = as.integer(b[["y"]]),
    pred_class = as.integer(torch_max(predictions, dim = 2)[[2]]),
    image = b[["path"]],
    p_trump = as.double(nnf_softmax(predictions, dim = 2)[, 2]),
    log_p_trump = as.double(predictions[, 2]),
    batch_loss = as.double(loss$item())
  )
  # add number of correct classifications in this batch to the aggregate
  correct <- sum(results[["pred_class"]] == results[["actual_class"]])
  list(results = results, total = labels$size(1), correct = correct)
}

# Generate code to create batch-processing loop with given symbols
process_batches <- function(dl, ...) {
  dots <- as.list(substitute(list(...)))[-1L]
  dl <- substitute(dl)
  bquote(
    {
      coro::loop(for (b in .(dl)) {
        ..(dots)
      })
    },
    splice = TRUE
  )
}

# Plot image tensor - mostly copied from
# https://blogs.rstudio.com/ai/posts/2020-10-19-torch-image-classification/
inspect_image_batch <- function(batch, plot_dims = c(4, 4), class_names = CLASS_NAMES) {
  # Plot dimensions are row by columns
  classes <- class_names[torch::as_array(batch[[2]])]
  images <- torch::as_array(batch[[1]]) %>%
    aperm(perm = c(1, 3, 4, 2))
  images <-
    (RESNET18_CONSTANTS$std * images + RESNET18_CONSTANTS$mean) * 255
  images[images > 255] <- 255
  images[images < 0] <- 0

  par(mfcol = plot_dims, mar = rep(1, 4))

  # replace raw labels with descriptive names, e.g. no_trump -> "No Trump"
  images %>%
    purrr::array_tree(1) %>%
    purrr::set_names(names(class_names[match(classes, class_names)])) %>%
    purrr::map(as.raster, max = 255) %>%
    purrr::iwalk(~ {
      plot(.x)
      title(.y)
    })
}



# Replace argument values in character vector of command-line args with standard names from a named vector
standardize_args <- function(args, mapping) {
  args <- unlist(strsplit(args, "\\s+"))
  identified <- args %in% names(mapping)
  args[identified] <- mapping[args[identified]]
  args
}

# Takes a character vector of arguments and parses
# one by one. Names are looked up in a list containing the expected type, which determines the parsing method
parse_args <- function(args, types) {
  included <- intersect(args, names(types))
  mandatory <- names(types)[sapply(types, function(x) !"default" %in% names(x))]
  if (length(mandatory) > 0 && !all(included %in% mandatory)) stop("Value missing for mandatory argument")
  # Only possible if none mandatory
  if (length(included) == 0) {
    return(lapply(types, function(x) x[["default"]]))
  }

  out <- vector("list", length(types))
  names(out) <- names(types)

  # Reduce charcter vector of arguments. If an argument is matched, record TRUE if its type is logical, otherwise consume next element (the value)
  while (length(args) > 0) {
    current <- args[[1]]
    args <- args[-1]
    # If arg type is logical, just record its presence.
    # Otherwise, next element contains value
    if (current %in% included) {
      type <- types[[c(current, "type")]]
      # Horrible special case of arg with default being last in list
      if (length(args) == 0 && type != "logical") {
        stop("Missing argument to ", current)
        # if (is.null((default <- types[[c(current, "default")]]))) {
        # break
        # } else {
        # out[[current]] <- default
        # }
      } else {
        out[[current]] <- switch(type,
          logical = TRUE,
          character = args[[1]],
          integer = as.integer(args[[1]]),
          double = as.double(args[[1]]),
          stop("Unhandled type ", type)
          # If needed: add nargs to arg types list:
          # Have switch expression instead consume that many args
          # Add line here deleting that many args
        )
      }
    }
    # Substitute defaults for omitted arguments
    missing <- setdiff(names(types), included)
    if (length(missing) > 0) {
      out[missing] <- lapply(types[missing], function(x) x[["default"]])
    }
  }
  out
}

# Records classification performance on a test set
# Args passed via ... available to expression
aggregate_cnn_results <- function(..., model_loop, aggregation_df = CNN_STATISTICS) {
  total <- correct <- 0
  results <- aggregation_df
  eval(model_loop, enclos = parent.frame())
  list(results = results, total = total, correct = correct)
}

confusion_matrix <- function(actual, predicted, class_names = CLASS_NAMES) {
  table(factor(class_names[actual], levels = class_names),
    factor(class_names[predicted], levels = class_names),
    dnn = c("truth", "predicted")
  )
}

compute_loss_weights <- function(labels, weights, class_names = CLASS_NAMES) {
  if (length(unique(labels)) != length(weights)) stop("Weights vector must have the same length as the number of unique class labels")
  loss_weights <- 1 / prop.table(table(labels)) * weights
  names(loss_weights) <- unname(class_names)
  loss_weights
}

# Use Dropbox API to get share links for all images in folder and save results to .csv
store_image_share_links <- function() {
  source(here::here("R", "classifier_utilities.R"))
  source(here::here("R", "dropbox_utilities.R"))

  ocr_results <- readRDS(here::here("data", "classifier", "outputs", "ocr_results.Rds"))
  cnn_results <- read.csv(here::here("data", "classifier", "outputs", "cnn_all_classifications.csv"))
  combined <- merge(ocr_results, cnn_results, by = "image")

  dropbox_token <- dropbox_authenticate()
  dropbox_folder <- "images"
  dropbox_paths <- gsub(
    paste0(
      ".*(",
      dropbox_folder, ".*)"
    ),
    "\\1",
    combined[["image"]]
  )
  combined[["url"]] <- sapply(dropbox_paths, dropbox_create_share_link, dropbox_token = dropbox_token)
  write.csv(data.frame(image = combined[["image"]], url = combined[["url"]]), here::here("data", "classifier", "image_urls.csv"), row.names = FALSE)
}
