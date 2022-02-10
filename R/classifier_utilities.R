library(torch)
library(torchdatasets)
library(torchvision)

# Much of this copied from R torch official site (https://blogs.rstudio.com/ai/posts/2020-10-19-torch-image-classification/)

# apparently expected by pretrained classifier

RESNET18_CONSTANTS <- list(mean = c(0.485, 0.456, 0.406), std = c(0.229, 0.224, 0.225))

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
  img <- img[, , -4] %>%
    # first convert image to tensor
    transform_to_tensor() %>%
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
  }
  # normalize according to what is expected by resnet
  do.call(transform_normalize, c(img, RESNET18_CONSTANTS))
}


