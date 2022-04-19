source(here::here("R", "utilities.R"))
# Import =======================================================================
winred_dat <- loadRData(
  here("data", "raw", "2022", "winred", "winred_text_scraped.Rda")
) %>%
  mutate(
    name_clean = gsub("_{1,}$", "", gsub("_{2,}", "_", trimws(name_clean)))
  )

summary(winred_dat)

## The images we need are linked in the following columns: logo and bgimg
## NOT og_image and twitter_image

# Logo downloads ===============================================================
logo_dat <- winred_dat %>%
  select(name_clean, logo) %>%
  filter(!is.na(logo) & !(logo == ""))

assert_that(!any(duplicated(logo_dat$name_clean)))

download_image_to_dir(
  dat = bg_dat, path = here("data/raw/2022/winred/logo"),
  image = "logo", name = "name_clean", identifier = "_logo_"
)
# bgimg downloads ==============================================================
bg_dat <- winred_dat %>%
  select(name_clean, bgimg) %>%
  filter(!is.na(bgimg) & !(bgimg == ""))

# Updated to use newer version of image download function
assert_that(!any(duplicated(bg_dat$name_clean)))
download_image_to_dir(
  dat = bg_dat, path = here("data/raw/2022/winred/bgimg"),
  image = "bgimg", name = "name_clean", identifier = "_bgimg_"
)

# Currently only Trump images
extra_image <- read.csv(here("data/classifier/extra_image/extra_image.csv"))
assert_that(!anyDuplicated(extra_image$url))
download_image_to_dir(
  dat = extra_image, path = here("data/classifier/trump"),
  image = "url", name = "name", identifier = "_extra_"
)
