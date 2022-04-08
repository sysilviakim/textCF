source(here::here("R", "classifier_utilities.R"))
ocr_results <- readRDS(here::here("data", "classifier", "outputs", "ocr_results.Rds"))
cnn_results <- read.csv(here::here("data", "classifier", "outputs", "cnn_results.csv"))
latest_date <- max(cnn_results[["date"]])
# cnn_results <- cnn_results[cnn_results[["date"]] == latest_date &
# cnn_results[["run"]] == max(cnn_results[["run"]][cnn_results[["date"]] == latest_date]), ]
cnn_results <- read.csv(here::here("data", "classifier", "outputs", "cnn_all_classifications.csv"))
# cnn_results[["image"]] <- basename(cnn_results[["image"]])
# ocr_results[["image"]] <- basename(ocr_results[["image"]])

combined <- merge(ocr_results, cnn_results, by = "image")
with(combined, table(cnn = CLASS_NAMES[pred_class], ocr = ocr_pred_class))

combined <- within(combined, prediction_category <- ifelse(actual_class == ocr_pred_class,
  ifelse(pred_class == actual_class, "both", "ocr"),
  ifelse(pred_class == actual_class, "cnn", "neither")
))

table(combined[["prediction_category"]])
disagreements <- combined[["prediction_category"]] != "both"

# Proportion where at least one classifier was incorrect
mean(disagreements)
# Error rate from naiively predicting the more common class
1 - max(prop.table(table(combined[["actual_class"]])))

inspect_images_from_paths(
  paths = combined[disagreements, "image"],
  labels = paste("Correctly classified by:", combined[disagreements, "prediction_category"]),
  plot_dims = c(2, 2)
)
