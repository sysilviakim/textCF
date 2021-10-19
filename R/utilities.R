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
library(stringdist)
library(caret)

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

actblue_wrangle <- function(input) {
  ## Unused parts ==============================================================
  js_rest <- input$js_rest
  post_donation_upsells <- input$post_donation_upsells %>% bind_rows()

  ## Federal candidates ========================================================
  df <- input %>%
    select(-js_rest, -eligibility_values_es, -post_donation_upsells) %>%
    bind_cols(., js_rest) %>%
    clean_names() %>%
    rename(id_main = id, kind_main = kind) %>%
    unnest(cols = c(entities), names_repair = "unique") %>%
    filter(kind == "candidate" & `federal?` == TRUE) %>%
    clean_names() %>%
    dedup() %>%
    select(
      -displayname_with_article,
      -contains("contribution_blurb_bottom"),
      -contains("eligibility_values"),
      -contains("_es$"),
      -contains("disclaimer_flags"),
      -contains("radio_amounts")
    ) %>%
    rowwise() %>%
    filter(!is.null(managing_entity)) %>%
    mutate(
      managing_id = managing_entity$id,
      managing_name = managing_entity$display_name,
      name_diff = stringdist(display_name, managing_name)
    ) %>%
    ungroup() %>%
    select(
      url, id, display_name, managing_id, managing_name, name_diff,
      display_pretty_location, everything()
    ) %>%
    ## name_diff is 5 or more
    select(-name_diff) %>%
    select(-managing_entity) %>%
    rename(id_x = id, display_name_x = display_name) %>%
    unnest(cols = c(brandings), names_repair = "unique") %>%
    clean_names() %>%
    dedup() %>%
    rename(
      id_brandings = id, display_name_brandings = display_name,
      id = id_x, display_name = display_name_x
    )
  
  ## Unnesting =================================================================
  temp <- nested_df(df) %>%
    clean_names()
  
  ## Twice to make sure (double- or triple-nested, sometimes)
  temp <- nested_df(temp) %>%
    clean_names()
  
  ## Kick out aesthetics e.g., border, padding, background, font
  temp <- temp %>%
    select(-contains("border_")) %>%
    select(-contains("padding_")) %>%
    select(-contains("text_")) %>%
    select(-contains("font")) %>%
    select(-contains("margin")) %>%
    select(-contains("color")) %>%
    select(-contains("alignment")) %>%
    select(-contains("letter_")) %>%
    select(-contains("space_")) %>%
    select(-contains("css")) %>%
    select(-contains("height"))
  
  assert_that(
    temp %>%
      map_chr(class) %>%
      {
        which(. == "data.frame")
      } %>% length() == 0
  )
  
  ## Entities vs. managing entities ============================================
  temp <- temp %>%
    filter(display_name == managing_name)
  
  ## Sanity checks =============================================================
  ## Must be political
  assert_that(all(temp$political == TRUE))
  
  ## Is not charity
  assert_that(all(temp$charity == FALSE))
  
  ## Not c4, i.e., 501(c)
  assert_that(all(temp$c4 == FALSE))
  
  ## Donate is all TRUE
  assert_that(all(temp$donate == TRUE))
  
  ## Drop unnecessary variables
  actblue_federal <- temp %>%
    select(-federal, -political, -charity, -c4, -kind, -managing_name)
  
  ## More cleaning, including amounts converted to dollar ======================
  actblue_federal <- actblue_federal %>%
    clean_names() %>%
    mutate(contribution_limit = contribution_limit / 100) %>%
    select(
      -contains("background_image_url"), -contains("header_image_url"),
      -contains("acceptable_card_types"), -contains("ticket_types"),
      everything()
    )
  
  ## Delete unnecessary phrases from candidate name ============================
  actblue_federal <- actblue_federal %>%
    filter(!grepl("2016|Marijuana Reform Party PAC", display_name)) %>%
    rowwise() %>%
    mutate(
      display_name = gsub(" for Congress$| - Federal$", "", display_name),
      display_name = gsub("\\(MA 04\\)| \\(\\)", "", display_name),
      display_name = gsub("^Friends of ", "", display_name),
      display_name = trimws(
        stringi::stri_trans_general(display_name, "latin-ascii")
      )
    )
  
  return(actblue_federal)
}

actblue_select_text <- function(input) {
  df <- input %>%
    actblue_wrangle() %>%
    ## Some important variables, but not for this project
    ## df %>% map(class) %>% {. == "list"} %>% which() %>% names()
    select(-where(is.list))
  
  names(df)[nearZeroVar(df, freqCut = 99.5 / 0.5)]
  #  [1] "date"                              "donate"                           
  #  [3] "full_background"                   "year"                             
  #  [5] "party"                             "recurring_interval"               
  #  [7] "deleted"                           "merchandise"                      
  #  [9] "apple_pay_merchant_identifier"     "tip_variant"                      
  # [11] "single_paypal_account"             "single_paypal_account_with_tip"   
  # [13] "show_tip_ask"                      "requires_employer"                
  # [15] "requires_employer_address"         "requires_eligibility_confirmation"
  # [17] "requires_cvv"                      "entry_mode"                       
  # [19] "locale"                            "accepts_paypal"                   
  # [21] "self_match_enabled"                "limited_info_available"           
  # [23] "political_2"                       "embedded_form"                    
  # [25] "allow_contact_sharing_opt_out"     "background_image_url_17"
  
  df %>%
    select(-names(df)[nearZeroVar(df, freqCut = 99.5 / 0.5)]) %>%
    select(
      display_name, url, fec_id_cand, first_name, last_name,
      title, contribution_blurb
    )
}

winred_select_text <- function(input) {
  df <- cong_text$senate$winred
  names(df)[nearZeroVar(df, freqCut = 99.5 / 0.5)]
  #  [1] "organization_revv_uid"           "viewport"                       
  #  [3] "stripe_publishable_key"          "update_cart_domain"             
  #  [5] "preview_new_proration_domain"    "preview_change_proration_domain"
  #  [7] "revv_api_domain"                 "page_donor_buckets_enabled"     
  #  [9] "organization_uid"                "organization_name"              
  # [11] "og_site_name"                    "og_locale"                      
  # [13] "og_type"                         "twitter_card"                   
  # [15] "og_image_width"                  "og_image_height"                
  # [17] "csrf_param"                      "date"                           
  # [19] "year"                            "party"
  
  df %>%
    mutate(contribution_blurb = paste(text, og_description, sep = " \n ")) %>%
    select(
      display_name = description, url = og_url, fec_id_cand = FEC_ID_cand,
      first_name, last_name,  title = og_title, contribution_blurb
    )
  
  ## Note that unlike ActBlue, WinRed same-link *did* change content!!
  ## https://secure.winred.com/marco-rubio-for-senate/website
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


