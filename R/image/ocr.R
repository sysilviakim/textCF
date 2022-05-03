source(here::here("R", "utilities.R"))
source(here::here("R", "image", "classifier_utilities.R"))
library(tesseract)

set.seed(1)
images <- list.files(here::here("data", "classifier", "images"), full.names = TRUE, recursive = TRUE)
labels <- basename(dirname(images))

# Characters to include
whitelist <- paste(c(letters, LETTERS, seq(0, 9), ".?!,+-"), collapse = "")

# Some options that looked potentially helpful actually do worse than defaults, however
tesseract_opts <- list(
  tessedit_char_whitelist = whitelist,
  textord_space_size_is_variable = TRUE,
  textord_heavy_nr = TRUE,
  textord_xheight_error_margin = .2,
  words_default_fixed_limit = .8
)

tesseract_params()[["desc"]][match(names(tesseract_opts), tesseract_params()[["param"]])]

engine <- tesseract()
ocr_results <- data.frame(
  ocr_output = I(lapply(images, ocr_data, engine = engine)),
  image = images
)

ocr_results[["ocr_text"]] <- sapply(ocr_results[["ocr_output"]], function(x) {
  paste(x[["word"]],
    collapse = " "
  )
})

ocr_results[["ocr_pred_class"]] <- sapply(
  ocr_results[["ocr_output"]],
  function(x) {
    if (nrow(x) == 0) {
      FALSE
    } else {
      any(grepl("trump", x[["word"]],
        ignore.case =
          TRUE
      ))
    }
  }
) + 1

# Prefer RDS over csv because of list column
saveRDS(ocr_results, here::here("data", "classifier", "outputs", "ocr_results.Rds"))
