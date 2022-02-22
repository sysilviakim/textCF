library(torch)
library(torchvision)
source(here::here("R", "utilities.R"))
source(here::here("R", "classifier_utilities.R"))

# Much of this is copied from
# https://blogs.rstudio.com/ai/posts/2020-10-19-torch-image-classification/
# This script will take a few minutes to run
data_dir <- here::here("data", "classifier", "trump_image")

train_ds <- image_folder_dataset(
  file.path(data_dir, "train"),
  transform = function(x) transform_image(x, train = TRUE)
)
test_ds <- image_folder_dataset(
  file.path(data_dir, "test"),
  transform = function(x) transform_image(x, train = FALSE)
)
valid_ds <- image_folder_dataset(
  file.path(data_dir, "valid"),
  transform = function(x) transform_image(x, train = FALSE)
)

batch_size <- 16

train_dl <- dataloader(train_ds, batch_size = batch_size, shuffle = TRUE)
valid_dl <- dataloader(valid_ds, batch_size = batch_size)
test_dl <- dataloader(test_ds, batch_size = batch_size)

# For first attempt,
# just use preset weights and modify ResNet's output layer for two classes
model <- model_resnet18(pretrained = TRUE)

# grad indicates used to compute gradient for each model pass,
# which we aren't doing
# Note this modifies in place
model$parameters %>% purrr::walk(function(param) param$requires_grad_(FALSE))
model$fc <- nn_linear(in_features = model$fc$in_features, out_features = length(CLASS_NAMES))

# In-place modification
model <- model$to(device = DEVICE)

weights <- torch_tensor(1 / prop.table(table(train_ds$targets))) * c(1, .3)
criterion <- nn_cross_entropy_loss(weight = weights) # add weight rescaling tensor here (inverse proportions); by default, averages loss
optimizer <- optim_sgd(model$parameters, lr = 0.1, momentum = 0.9)

# I try a learning rate of about 10^-6.
# Highly unstable due to few images with Trump
lrs <- find_lr(train_dl, optimizer) %>%
  suppressMessages()
# ggplot(aes(log_lrs, losses)) +
# geom_point(size = 1) +
# theme_classic()

# Find loss-minimizing learning rate
lr <- with(lrs, log_lrs[which.min(losses)])

num_epochs <- 10
scheduler <- optimizer %>%
  lr_one_cycle(
    max_lr = lr, epochs = num_epochs, steps_per_epoch = train_dl$.length()
  )

# Partially substituted expressions to reduce duplication
train_loop <- process_batches(train_dl, train_batch, train_losses)
valid_loop <- process_batches(valid_dl, valid_batch, valid_losses)
test_loop <- process_batches(test_dl, test_batch, test_losses)

for (epoch in seq(num_epochs)) {
  model$train()
  train_losses <- c()

  eval(train_loop)

  model$eval()
  valid_losses <- c()

  eval(valid_loop)

  cat(
    sprintf(
      "\nLoss at epoch %d: training: %3f, validation: %3f\n",
      epoch, mean(train_losses), mean(valid_losses)
    )
  )
}

test_losses <- c()
total <- 0
correct <- 0
results <- data.frame(actual = integer(), predicted = integer())
eval(test_loop)

# High accuracy, but not compelling because very few images feature Trump
test_losses
correct / total
# Confusion matrix
table(factor(CLASS_NAMES[results$actual], levels = CLASS_NAMES), factor(CLASS_NAMES[results$predicted], levels = CLASS_NAMES), dnn = colnames(results))
