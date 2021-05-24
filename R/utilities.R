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
nested_df <- function(df, var = NULL) {
  ## broad applications of tidyr::unnest can be problematic
  if (length(var) > 1) {
    stop("Specify one variable at each run.")
  }
  if (!is.null(var)) {
    temp <- df[[var]]
    if (nrow(temp) == nrow(df)) {
      return(bind_cols(df %>% select(-!!as.name(var)), temp))
    } else {
      message(paste0("Number of rows do not match for variable ", var))
    }
  } else {
    var_list <- df %>% map_chr(class) %>% {which(. == "data.frame")} %>% names()
    for (var in var_list) {
      temp <- df[[var]]
      if (nrow(temp) == nrow(df)) {
        df <- bind_cols(df %>% select(-!!as.name(var)), temp)
        message(paste0("Successfully unnested variable ", var))
      } else {
        message(paste0("Number of rows do not match for variable ", var))
      }
    }
    return(df)
  }
}

image_download_logo_updated <- function(dat, image, name) {
  path <- here("data/raw/2022/winred/logo")
  if (!dir.exists(path)) dir.create(path)

  for (i in 1:nrow(dat)) {
    download.file(
      url = dat[[image]][i],
      destfile = file.path(
        path,
        paste0(
          gsub("_{1,}$", "", gsub("_{2,}", "_", trimws(dat[[name]][i]))),
          "_logo_", format(Sys.Date(), "%Y%m%d"), ".jpg"
        )
      ),
      method = "curl"
    )
    Sys.sleep(3)
  }
}

image_download_bgimg_updated <- function(dat, image, name) {
  path <- here("data/raw/2022/winred/bgimg")
  if (!dir.exists(path)) dir.create(path)
  
  for (i in 1:nrow(dat)) {
    download.file(
      url = dat[[image]][i],
      destfile = file.path(
        path,
        paste0(
          gsub("_{1,}$", "", gsub("_{2,}", "_", trimws(dat[[name]][i]))),
          "_bgimg_", format(Sys.Date(), "%Y%m%d"), ".jpg"
        )
      ),
      method = "curl"
    )
    Sys.sleep(3)
  }
}

# Other options ================================================================
