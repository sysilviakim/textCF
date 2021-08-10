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

image_download <- function(dat, image, name, pathway) {
  path <- here(pathway)
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

fb_query_short <- function(idx, 
                           fields = "ad_data",
                           ad_delivery_date_max = "2020-11-30",
                           limit = 1e7) {
  query <- adlib_build_query(
    ad_reached_countries = 'US', 
    ad_active_status = 'ALL',
    ad_type = "POLITICAL_AND_ISSUE_ADS",
    ## Notice that for list creation purposes, idx is still character
    search_page_ids = as.numeric(idx),
    fields = fields,
    ad_delivery_date_max = ad_delivery_date_max,
    limit = limit
  )
}

# Other options ================================================================
fb_fields <- c(
  # "ad_data", "demographic_data", "region_data", 
  "ad_creation_time", "ad_creative_body", "ad_creative_link_caption",
  "ad_creative_link_description", "ad_creative_link_title",
  "ad_delivery_start_time", "ad_delivery_stop_time",
  "ad_snapshot_url", "currency", "demographic_distribution", "funding_entity",
  "impressions", "page_id", "page_name", "potential_reach", 
  "publisher_platforms", "region_distribution", "spend"
)

## https://facebookresearch.github.io/Radlibrary/index.html
## See also
## https://disinfo.quaidorsay.fr/en/facebook-ads-library-assessment


