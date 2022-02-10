# From a directory containing images, creates a new directory where each image is stroed in a subbdirectory
# corresponding to its class - the structure expected by Torch

source(here::here("R", "utilities.R"))
img_dir <- here::here("data", "raw", "2022", "winred")
stopifnot(dir.exists(img_dir))

dest_dir <- here::here("data", "classifier")
classes <- c("no_trump", "trump")

# Need to recreate names assigned to images so we can match the correct entries of the
# vector of class labels with the correct images.
# Luckily, the name columns in these two tables have
# no disjoint elements
name_ref <- loadRData(
  here("data", "raw", "2022", "winred", "winred_text_scraped.Rda")
) %>%
  mutate(
    name_clean = gsub("_{1,}$", "", gsub("_{2,}", "_", trimws(name_clean)))
  ) %>%
  select(name, name_clean)

# Have to manually replace one name, likely corrupted by accent mark character
class_ref <- read.csv(here::here(img_dir, "winred_images.csv")) %>%
  select(-name_clean) %>%
  left_join(name_ref, by = "name") |>
  mutate(name_clean = replace_na(name_clean, "committee_to_elect_erin_paré"))

images <- Sys.glob(file.path(img_dir, "bgimg", "*.png"))

# Extract names of images to identify corresponding rows in dataset containing metadata for each image
# 16 of 696 images failed to download, so can't use positional matching
image_names <- str_match(images, "/(\\w+)_bgimg")[, 2]
class_ref <- filter(class_ref, name_clean %in% image_names) %>%
  pull(bgimg_trump)

# One-hot ecnoding, so add 1 to index levels
class_ref <- classes[1 + class_ref]
# Only 10 positive labels?
print(class_ref)
if (na_img <- sum(is.na(class_ref)) > 0) {
  print("Note: Class is NA for", na_img, "images", "\n")
}
table(class_ref)
images <- split(images, class_ref)

if (!dir.exists(dest_dir)) {
  dir.create(dest_dir)
}
subdirs <- file.path(dest_dir, classes)
for (i in seq_along(classes)) {
  if (!dir.exists(subdirs[[i]])) {
    dir.create(subdirs[[i]])
    cat("Created", subdirs[[i]], "\n")
  }
  file.copy(images[[classes[[i]]]], subdirs[[i]])
  cat("Copied", length(images[[classes[[i]]]]), "file(s)", "\n")
}
