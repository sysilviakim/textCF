# Tidyverse ====================================================================
library(plyr)
library(tidyverse)
library(lubridate)

# Text analysis packages =======================================================
library(quanteda)
library(quanteda.textplots)
library(quanteda.textstats)

# Other packages ===============================================================
library(here)
library(janitor)
library(assertthat)

# Non-CRAN packages ============================================================
library(Kmisc)

# Functions ====================================================================
image_download_logo_updated <- function(dat, image, name) {
  path <- here("data/raw/2022/winred/logo")
  if (!dir.exists(path)) dir.create(path)

  for (i in 1:nrow(dat)) {
    filepath[i] <- file.path(path, paste0(name[i], "_logo.jpg"))
    download.file(
      url = image[i],
      destfile = filepath[i],
      method = "curl"
    )
  }
}


# Other options ================================================================
