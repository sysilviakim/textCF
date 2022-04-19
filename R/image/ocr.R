source(here::here("R", "utilities.R"))
source(here::here("R", "classifier_utilities.R"))
library(tesseract)

set.seed(1)
images <- list.files(here::here("data", "classifier", "no_trump"), full.names = TRUE) %>%
  # sample(30, replace = FALSE) %>%
  c(list.files(here::here("data", "classifier", "trump"), full.names = TRUE))
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

classified <- lapply(images, ocr, engine = tesseract(options = tesseract_opts))
names(classified) <- CLASS_NAMES[grepl("trump", classified, ignore.case = TRUE) + 1]

table(labels, names(classified))
