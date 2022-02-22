library(torch)
library(torchdatasets)
library(torchvision)

# Much of this copied from R torch official site
# (https://blogs.rstudio.com/ai/posts/2020-10-19-torch-image-classification/)

# apparently expected by pretrained classifier

RESNET18_CONSTANTS <-
  list(mean = c(0.485, 0.456, 0.406), std = c(0.229, 0.224, 0.225))
DEVICE <- if (cuda_is_available()) torch_device("cuda:0") else "cpu"
CLASS_NAMES <- c("No Trump" = "no_trump", "Trump" = "trump")
# Appropriate functions to read different image file types
IMAGE_FUNS <- list(
  "jpg" = jpeg::readJPEG,
  "jpeg" = jpeg::readJPEG, "png" = png::readPNG
)

# Subclass dataset function to hold images for analysis
image_dataset <- function(img_path, root, y) {
  stopifnot(dir.exists(img_path))
  if (!dir.exists(root)) {
    dir.create(root)
  }
  self$y <- torch_tensor(as.integer(y))
  .getitem <- function(i) list(x = self$x[, i], y = self$y[i])
  .length <- function() self$y$size[[1]]
}

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
# Quick way of making square images for plotting
pad_matrix <- function(mat, fill = "#FFFFFF") {
  stopifnot("Fill must match type of matrix" = typeof(mat) == typeof(fill))
  dims <- dim(mat)
  out <- if ((difference <- dims[[1]] - dims[[2]]) > 0) {
    cbind(mat, matrix(fill, nrow = dims[[1]], ncol = difference))
  } else if (difference < 0) {
    rbind(mat, matrix(fill, nrow = -difference, ncol = dims[[2]]))
  } else {
    mat
  }
  as.raster(mat)
}

read_image <- function(path, to_raster = TRUE) {
  fun <- switch(tools::file_ext(path),
    "jpg" = ,
    "jpeg" = jpeg::readJPEG,
    "png" = png::readPNG,
    {
      stop("Unknown image format")
    }
  )
  out <- fun(path)
  if (to_raster) {
    out <- as.raster(out)
  }
  out
}

# Plots all images stored in a directory, chunked into plots of given dimension (default 4 x 4).
# Crops to square dimensions
# May cause memory error if given too many images
inspect_image_directory <- function(path, plot_dims = c(4, 4), shuffle = FALSE) {
  size <- prod(plot_dims)
  images <- list.files(path, full.names = TRUE)
  images <- images[tools::file_ext(images) %in% names(IMAGE_FUNS)]
  if (shuffle) {
    images <- images[sample(length(images), length(images), replace = FALSE)]
  }
  n_plots <- ceiling(length(images) / size)
  old <- par(c("mar", "mfcol"))
  par(mfcol = plot_dims, mar = rep(1, 4))

  # Split list into chunks, each
  images <- setNames(images, basename(images)) %>%
    split(rep(seq(n_plots), each = size)[seq_along(images)])

  # Recursively traverses possibly nested list of image rasters and plots them
  plot_image_list <- function(lst, pause = 0) {
    if (is.list(lst[[1]])) {
      lapply(lst, plot_image_list, pause)
    } else {
      mapply(function(img, name) {
        plot(pad_matrix(img))
        title(name)
      }, lst, names(lst))
      Sys.sleep(pause)
    }
  }

  lapply(images, function(x) {
    lapply(x, read_image) %>%
      plot_image_list(pause = 3)
  })
  # Reset plot parameters
  on.exit(do.call(par, old))

  # Recursion needed because images consists of multiple sublists if it has more elements
  # than a single plot can fit
  # invisible(plot_image_list(images, pause = 3))
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

train_batch <- function(b) {
  optimizer$zero_grad()
  output <- model(b[[1]])
  loss <- criterion(output, b[[2]]$to(device = DEVICE))
  loss$backward()
  optimizer$step()
  scheduler$step()
  loss$item()
}

valid_batch <- function(b) {
  output <- model(b[[1]])
  loss <- criterion(output, b[[2]]$to(device = DEVICE))
  loss$item()
}

test_batch <- function(b) {
  output <- model(b[[1]])
  labels <- b[[2]]$to(device = DEVICE)
  loss <- criterion(output, labels)

  test_losses <<- c(test_losses, loss$item())
  # torch_max returns a list, with position 1 containing the values
  # and position 2 containing the respective indices
  predicted <- torch_max(output$data(), dim = 2)[[2]]
  results <<- rbind(results, data.frame(actual = as.integer(b$y), predicted = as.integer(predicted)))
  total <<- total + labels$size(1)
  # add number of correct classifications in this batch to the aggregate
  correct <<- correct + (predicted == labels)$sum()$item()
}

# Generate code to create batch-processing loop with given symbols
process_batches <- function(dl, batch_fun, loss_vec) {
  dl <- substitute(dl)
  batch_fun <- substitute(batch_fun)
  loss_vec <- substitute(loss_vec)
  substitute({
    coro::loop(for (b in dl) {
      loss <- batch_fun(b)
      loss_vec <- c(loss_vec, loss)
    })
  })
}

# Plot image tensor - mostly copied from
# https://blogs.rstudio.com/ai/posts/2020-10-19-torch-image-classification/
inspect_image <- function(batch, plot_dims, class_names = CLASS_NAMES) {
  # Plot dimensions are row by columns
  classes <- batch[[2]]
  images <- torch::as_array(batch[[1]]) %>%
    aperm(perm = c(1, 3, 4, 2))
  images <-
    (RESNET18_CONSTANTS$std * images + RESNET18_CONSTANTS$mean) * 255
  images[images > 255] <- 255
  images[images < 0] <- 0

  par(mfcol = plot_dims, mar = rep(1, 4))

  images %>%
    purrr::array_tree(1) %>%
    purrr::set_names(names(class_names[torch::as_array(classes)])) %>%
    purrr::map(as.raster, max = 255) %>%
    purrr::iwalk(~ {
      plot(.x)
      title(.y)
    })
}
