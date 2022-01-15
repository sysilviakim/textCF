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
image_download_logo_updated(logo_dat, image = "logo", name = "name_clean")

# bgimg downloads ==============================================================
bg_dat <- winred_dat %>%
  select(name_clean, bgimg) %>%
  filter(!is.na(bgimg) & !(bgimg == ""))

assert_that(!any(duplicated(bg_dat$name_clean)))
image_download(
  dat = bg_dat, image = "bgimg", name = "name_clean",
  pathway = "data/raw/2022/winred/bgimg"
)
