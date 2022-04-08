#! /usr/bin/env Rscript
sink(file = "/dev/null", type = "output")
library(torch)
library(torchvision)
library(luz)

source(here::here("R", "utilities.R"))
source(here::here("R", "classifier_utilities.R"))
source(here::here("R", "candidate_image_dataset.R"))
source(here::here("R", "dropbox_utilities.R"))

#' Much of this is copied from [here](https://blogs.rstudio.com/ai/posts/2020-10-19-torch-image-classification/)
#' At present, this script uses the pretrained RESNET18 classifier to classify images as
#' either containing or not containing Donald Trump. It implements this network with the
#' `torch` package, an R interface to the `torch` machine learning library also employed by the
#' popular PyTorch framework. I chose `torch` because it does not introduce a Python dependency and
#' is easier to debug than `keras`, an alternative, since it uses less abstraction.
#' This script takes about 10 minutes to run on my machine.


#' Some complicated business here to parse command-line arguments. This makes it possible to call this script with different parameters.
torch_manual_seed(1)
args <- commandArgs(trailingOnly = TRUE)
# could add nargs to list
outputs_path <- here::here("data", "classifier", "outputs")
sink(file = NULL, type = "output")
args_lookup <- c(
  "--use_dropbox" = "use_dropbox",
  "-d" = "use_dropbox",
  "--save_results" = "save_results",
  "-s" = "save_results",
  "--img_dir" = "img_dir",
  "-i" = "img_dir",
  "--log_file" = "log_file",
  "-l" = "log_file",
  "--debug" = "debug",
  "--pretrained" = "pretrained",
  "-p" = "pretrained",
  "--help" = "help",
  "-h" = "help"
)
arg_types <- list(
  use_dropbox = list(
    type = "logical",
    default = FALSE
  ),
  save_results = list(
    type = "logical",
    default = FALSE
  ),
  img_dir = list(
    type = "character",
    default = NULL
  ),
  log_file = list(
    type = "character",
    default = NULL
  ),
  debug = list(
    type = "logical",
    default = FALSE
  ),
  pretrained = list(
    type = "logical",
    default = FALSE
  )
)

args <- standardize_args(args, args_lookup)
print(args)
if ("help" %in% args) {
  cat(
    "usage: R -f ./R/classifier.R  [--args [ARGS]...]", "\n",
    "Train and test a neural network using the",
    sQuote("torch"), "package on saved image data", "\n",
    "-s,  --save Save parameters and classification results to a .csv", "\n",
    "-i,  --img_dir Specify an alternate directory from which to take images to classify (default is",
    here::here("data", "classifier", "trump_image"), "if",
    sQuote("use_dropbox"), "is not specified, /trump_image otherwise", "\n",
    "-d, --use_dropbox Load images from a Dropbox folder instead of a local directory (requires OAuth identification)", "\n",
    "-l, --log_file Write program output to a specified file", "\n",
    "--debug If an error is encountered during model fitting, recover with browser (if run interactively) or dump frames (if not)", "\n",
    "--pretrained, -p Load and use trained model instead of training from scratch on available data", "\n",
    "--help, -h Print this message and exit", "\n"
  )
  quit(status = 0, save = "no")
}

arg_values <- parse_args(args, arg_types)
use_log <- "log_file" %in% args
if (use_log) sink(file = arg_values[["log_file"]], type = "output")

if (arg_values[["use_dropbox"]]) {
  default_dir <- "trump_image"
  token <- dropbox_authenticate(refresh = TRUE)
} else {
  default_dir <- here::here("data", "classifier", "trump_image")
  token <- NULL
}
if (is.null(arg_values[["img_dir"]])) arg_values[["img_dir"]] <- default_dir
cat("Argument values after substitution", "\n")
print(arg_values)

start_time <- Sys.time()
transform_params <- list(angle = 20, flip = .7, rescale_min = .9, rescale_max = 1.1)
sample_weights <- c(
  no_trump = 1,
  trump = 10
)
ds_call <- call("candidate_image_dataset",
  img_dir = quote(img_dir),
  sample_weights = sample_weights,
  transform_params = transform_params,
  use_dropbox = arg_values[["use_dropbox"]],
  dropbox_token = token,
  debug = arg_values[["debug"]],
  full_image_paths = TRUE
)
batch_size <- 32
train_ds <- eval(ds_call, envir = list(img_dir = file.path(arg_values[["img_dir"]], "train")))
loss_weights <- compute_loss_weights(as.numeric(train_ds$targets), c(0.8, 1))
criterion <- nn_cross_entropy_loss(weight = loss_weights) # add weight rescaling tensor here (inverse proportions); by default, averages loss
# Skip training if using pretrained model
if (arg_values[["pretrained"]]) {
  model <- torch_load(here::here("data", "classifier", "outputs", "cnn_trained.Rds"))
} else {
  model <- model_resnet18(pretrained = TRUE)
  valid_ds <- eval(ds_call, envir = list(img_dir = file.path(arg_values[["img_dir"]], "valid")))
  momentum <- 0.9

  train_dl <- dataloader(train_ds, batch_size = batch_size, shuffle = TRUE)
  valid_dl <- dataloader(valid_ds, batch_size = batch_size)

  #' These are the images contained in one batch. Note the rotations, flips, and resizings.
  batch <- train_dl$.iter()$.next()
  if (!use_log) inspect_image_batch(batch)

  #' This code modifies all model parameters to be ignored in gradient computations, freezing the
  #' pretrained weights. This saves a great deal of computing power.
  #' The one essential change is reshaping the output layer to yield two values, for the two classes.
  model$parameters %>% purrr::walk(function(param) param$requires_grad_(FALSE))


  # Idea borrowed from https://discuss.pytorch.org/t/how-to-delete-layer-in-pretrained-model/17648
  if (FALSE) {
    identity_module <- nn_module(
      classname = "Identity",
      initialize = function() {},
      forward = identity,
      print = function() cat("An identity module that forwards the output of the preceding layer", "\n")
    )
    model$fc <- identity_module()
  }
  # Alternate strategy: remove final layer, add ReLU, use as features for linear SVM or logistic model to combat overfitting (Razavian et al. 2014)
  model$fc <- nn_linear(in_features = model$fc$in_features, out_features = length(CLASS_NAMES))

  model <- model$to(device = DEVICE)

  #' I set a standard loss function and apply some weights to account for the class imbalance. This
  #' will increase the computed loss for the Trump class.

  optimizer <- optim_adam(params = model$parameters) # optim_sgd(model$parameters, lr = 0.1, momentum = momentum)

  #' Learning rate controls how fast the model updates weights. I use a function copied from
  #' the source cited above. The optimal rate seems to vary between model runs, perhaps due
  #' to the class imbalance.
  #' Each of the 10 epochs should take `r ceiling(length(train_ds))` batches to process all `r length(train_ds)` examples.
  lrs <- find_lr(train_dl, optimizer) %>%
    suppressMessages()

  if (!use_log && interactive()) {
    ggplot2::ggplot(lrs, aes(log_lrs, losses)) +
      ggplot2::geom_point(size = 1) +
      ggplot2::geom_line() +
      ggplot2::theme_classic() +
      ggplot2::labs(x = "Log-10 Learning Rate", y = "Loss", title = "Learning Rate Tuning")
  }
  # Find loss-minimizing learning rate ()
  learn_rate <- with(lrs, 10^log_lrs[which.min(losses)])
  num_epochs <- 10
  scheduler <- optimizer %>%
    lr_one_cycle(
      max_lr = .1, epochs = num_epochs, steps_per_epoch = train_dl$.length()
    )

  #' ## Training and Assessment
  #'
  #' This code trains the model and applies it to the test set. It naturally takes the longest to
  #' run. Since the `for` loops for each data partition are nearly identical, I wrote a function to
  #' substitute appropriate values into a generic expression. That would have been much harder in Python.
  #' As I write this, training loss is much higher than validation loss for most epochs, which is unusual for a model (like this one) without dropout layers.
  #' My best guess would be confusion between images of people and of text, or perhaps instability due to the class imbalance.
  train_loop <- process_batches(
    train_dl, loss <- train_batch(b,
      model = model,
      optimizer = optimizer,
      criterion = criterion,
      scheduler = scheduler
    ),
    train_losses <- c(train_losses, loss)
  )
  valid_loop <- process_batches(valid_dl, loss <- valid_batch(b,
    model = model,
    criterion = criterion
  ), valid_losses <- c(valid_losses, loss))

  for (epoch in seq(num_epochs)) {
    train_losses <- valid_losses <- double()
    model$train()

    eval(train_loop)

    model$eval()

    eval(valid_loop)

    cat(
      sprintf(
        "\nLoss at epoch %d: training: %3f, validation: %3f\n",
        epoch, mean(train_losses), mean(valid_losses)
      )
    )
  }

  torch_save(model, here::here("data/classifier/outputs/cnn_trained.Rds"))
}
model

# Change to default value if not specified in command-line args


test_ds <- eval(ds_call, envir = list(
  img_dir =
    file.path(arg_values[["img_dir"]], "test"), transform_params = NULL, sample_weights = NULL
))

test_loop <- process_batches(
  test_dl, out <- test_batch(b, model = model, criterion = criterion),
  results <- rbind(results, out[["results"]]),
  total <- total + out[["total"]],
  correct <- correct + out[["correct"]]
)
#' Right now I use the standard `dataloader` class, which creates an object capable of
#' retrieving observations in batches. A batch is the number of observations processed before
#' weights are updated. Several are processed each epoch (a complete cycle through each training observation).
test_dl <- dataloader(test_ds, batch_size = batch_size)

#' RESNET18 has a complicated architecture. A possible next step is implementing a simpler network from scratch.
#' There are millions of parameters: training them all would be expensive
model

total <- correct <- 0
results <- data.frame(
  actual_class = integer(),
  pred_class = integer(),
  image = character(),
  p_trump = double(),
  log_p_trump = double(),
  batch_loss = double()
)
results <- aggregate_cnn_results(test_dl, model_loop = test_loop)
end_time <- Sys.time()
elapsed <- round(end_time - start_time, digits = 3)
elapsed_unit <- attr(elapsed, "units")
elapsed <- as.numeric(elapsed)
cat("Training and evaluation time:", elapsed, elapsed_unit, "\n")
#' Accuracy overall is quite good, but again the class imbalance makes that a misleading measure (a "model" that just predicted "not Trump" every time would do well).
unique(results[[c("results", "batch_loss")]])
test_overall_accuracy <- with(results, correct / total)
test_overall_accuracy

#' The confusion matrix.
CM <- confusion_matrix(results[[c("results", "actual_class")]], results[[c("results", "pred_class")]])
test_precision <- CM["trump", "trump"] / sum(CM[, "trump"])
test_recall <- CM["trump", "trump"] / sum(CM["trump", ])

#' I establish a metric to track area under the ROC curve. This is roughly the proportion of
#' accurate classifications, usually defined as all fitted probabilities as prediction thresholds. I specify some thresholds myself.
roc_spec <- luz_metric_binary_auroc(thresholds = seq(.1, .9, .1))
test_roc_auc <- roc_spec$new()
test_roc_auc$update(torch_tensor(results[[c("results", "p_trump")]]), torch_tensor(results[[c("results", "actual_class")]])$subtract(1))
test_roc_auc <- test_roc_auc$compute()

if (arg_values[["save_results"]]) {
  local({
    metadata_path <- file.path(outputs_path, "cnn_latest_metadata.csv")
    summary_path <- file.path(outputs_path, "cnn_summary.csv")
    date <- Sys.Date()
    if (file.exists(metadata_path) &&
      (latest_metadata <- read.csv(metadata_path))[["date"]] == date) {
      run <- latest_metadata[["run"]] + 1
    } else {
      run <- 1
    }
    metadata <- data.frame(date = as.character(date), run = run)
    write.csv(metadata, metadata_path, row.names = FALSE)

    append_csv(data.frame(
      date = date,
      run = run,
      results[["results"]]
    ), file.path(
      outputs_path,
      "cnn_results.csv"
    ), row_names = FALSE)
    # Record parameters and overall results
    train_only_params <- mget(c("num_epochs", "learn_rate", "momentum"),
      envir = globalenv(), ifnotfound = list(NA_real_)
    )
    cnn_summary <- c(metadata,
      elapsed = elapsed,
      elapsed_unit = elapsed_unit,
      no_trump_sample_weight = sample_weights[["no_trump"]],
      trump_sample_weight = sample_weights[["trump"]],
      no_trump_loss_weight = loss_weights[["no_trump"]],
      trump_loss_weight = loss_weights[["trump"]],
      transform_params,
      batch_size = batch_size,
      train_only_params,
      test_overall_accuracy = test_overall_accuracy,
      test_precision = test_precision,
      test_recall = test_recall,
      test_roc_auc = test_roc_auc,
      recursive = FALSE
    ) %>%
      as.data.frame() %>%
      rapply(round, classes = "numeric", how = "replace", digits = 3)
    append_csv(
      cnn_summary,
      summary_path,
      row_names = FALSE
    )
  })
}

#' Unused code for fitting the model with `luz`, a higher-level `torch` interface.
# fitted <- net %>%
# setup(
# loss = criterion,
# optimizer = optimizer_adam,
# metrics = list(
# luz_metric_binary_accuracy,
# luz_metric_binary_auroc
# )
# ) %>%
# set_hprams(num_class = 10) %>%
# set_opt_hparams(lr = lr) %>%
# fit(train_dl, epochs = 10, valid_data = valid_dl)
if (use_log) sink(file = NULL, type = "output")
