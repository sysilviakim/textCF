# Simple script to classify all available images with a trained network and save the results
source(here::here("R", "utilities.R"))
source(here::here("R", "classifier_utilities.R"))
source(here::here("R", "candidate_image_dataset.R"))

model <- torch_load(here::here("data", "classifier", "outputs", "cnn_trained.Rds"))

ds <- candidate_image_dataset(
  img_dir = here::here("data", "classifier", "images")
)
dl <- dataloader(ds, batch_size = 32)

loss_weights <- compute_loss_weights(as.numeric(ds$targets), c(0.5, 1))
criterion <- nn_cross_entropy_loss(weight = loss_weights) # add weight rescaling tensor here (inverse proportions); by default, averages loss

fit_loop <- process_batches(
  dl, out <- test_batch(b, model = model, criterion = criterion),
  results <- rbind(results, out[["results"]]),
  total <- total + out[["total"]],
  correct <- correct + out[["correct"]]
)
results <- aggregate_cnn_results(d, model_loop = fit_loop)
write.csv(results[["results"]],
  here::here(
    "data", "classifier",
    "outputs", "cnn_all_classifications.csv"
  ),
  row.names = FALSE
)
