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

image_download_bgimg_updated(bg_dat, image = "bgimg", name = "name_clean")

##### Other Notes #####
# Going forward, one of the big things I need to tackle is sorting out how to
# download the images without hardcoding a file path. I think this should be
# a straightforward endeavor to undertake as part of the coding for ActBlue, one
# that can be re-applied to this code so that it can be more broadly useful.
# I'll also do more to better-familiarize myself with the 'here' package's uses,
# and I'll look into the 'styler' package as well.
