# Tidyverse ====================================================================
library(plyr)
library(tidyverse)
library(lubridate)
library(rvest)
library(haven)
library(glue)

# Text analysis packages =======================================================
library(quanteda)
library(quanteda.textplots)
library(quanteda.textstats)
library(quanteda.dictionaries)
## https://muellerstefan.net/files/quanteda-cheatsheet.pdf
library(lingmatch)

# Other packages===============================================================
library(here)
library(labelled)
library(ggpubr)
library(janitor)
library(assertthat)
library(stringdist)
library(jsonlite)
library(xml2)
library(caret)
library(RSelenium)
library(netstat)
library(Radlibrary)
library(knitr)
library(kableExtra)
library(stargazer)
library(RColorBrewer)
library(gender)

## devtools::install_github("hrbrmstr/wayback")
library(wayback)

# Non-CRAN packages ============================================================
library(Kmisc)

# Dictionaries =================================================================
## Moral Foundations Dictionary 2.0
## https://osf.io/ezn37/
moral <- dictionary(file = here("data/raw/dictionaries/mfd2.0.dic"))

## Trolling Dictionary
## https://osf.io/bm8uy/
troll <- read_csv(
  file = here(
    "data", "raw", "dictionaries",
    "troll_and_divide/troll_and_divide_Glove_Expansion_Raters_phase_2.csv"
  )
) %>%
  pull(Word) %>%
  list(troll = .) %>%
  dictionary()

## NRC dictionary
## Supplement with https://www.tidytextmining.com/sentiment.html?
NRC <- read.dic(here("data", "raw", "dictionaries", "NRC.dic"))
NRC <- dictionary(NRC)

# Functions ====================================================================
## Because of this function, now
## source(here("R/utilities.R"), encoding = 'UTF-8') will not work
## take the encoding out!
clean_candidate <- function(df, x = "candidate") {
  ## .Rda ---> .csv ---> .Rda errors
  ## this is double quotes being messed up ---> does not work
  # df[[x]] <- gsub("¬í", "", df[[x]])
  # df[[x]] <- gsub("í", "", df[[x]])
  # df[[x]] <- gsub("î", "", df[[x]])
  # ## this is accentuated alphabets messed up ---> also does not work
  # df[[x]] <- gsub("v°", "A", df[[x]])
  # df[[x]] <- gsub("v±", "N", df[[x]])
  # df[[x]] <- gsub("v<U+222B>", "U", df[[x]])
  # df[[x]] <- gsub("v©", "E", df[[x]])
  # df[[x]] <- gsub("v=", "O", df[[x]])
  # df[[x]] <- stringi::stri_trans_general(tolower(df[[x]]), "latin-ascii")
  ## This will intentionally kill all accentuated letters
  ## which butchers the name e.g., velazquez ---> velzquez, but will ensure
  ## consistency for matching
  df[[x]] <- gsub("[^ -~]", "", tolower(df[[x]]))
  df[[x]] <-
    gsub(
      "[ \t]{2,}", " ",
      gsub("^\\s+|\\s+$", "", gsub("[[:punct:]]", " ", df[[x]]))
    )
  ## delete middle names, perform twice
  df[[x]] <- gsub(
    gsub("\\s+", " ", paste(" ", letters %>% paste(collapse = " | "), " ")),
    " ", df[[x]]
  )
  df[[x]] <- gsub(
    gsub("\\s+", " ", paste(" ", letters %>% paste(collapse = " | "), " ")),
    " ", df[[x]]
  )
  return(df)
}

donate_classify <- function(x) {
  x %>%
    mutate(
      donate = case_when(
        grepl(
          paste0(
            "donate|donation|contribute|contribution|chip in|pitch in|",
            "PAC money|corporate PAC|dollar| bucks|\\$|chipping in|",
            "end-of-month deadline|end-of-quarter deadline|",
            "end-of-year deadline|match opportunity|match fund"
          ),
          tolower(ad_creative_body)
        ) ~ TRUE,
        TRUE ~ FALSE
      ),
      ## Overall Financial/non-Financial Classifier
      financial = case_when(
        grepl(
          paste0(
            "ActBlue|WinRed|NGP VAN|Anedot|Victory Passport|Misc.|",
            "Personal Contribution Link"
          ), type
        ) ~ "Donor-targeting",
        isTRUE(donate) ~ "Donor-targeting",
        grepl(
          "Voter-targeting|Non-financial|Government Information|Facebook Page",
          type
        ) ~ "Voter-targeting",
        is.na(type) ~ "Voter-targeting",
      )
    )
}

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
    var_list <- df %>%
      map_chr(class) %>%
      {
        which(. == "data.frame")
      } %>%
      names()
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

download_with_extension <- function(path, url, name, tag) {
  destfile <- file.path(
    path,
    paste0(
      gsub("_{1,}$", "", gsub("_{2,}", "_", trimws(name))),
      tag, format(Sys.Date(), "%Y%m%d")
    )
  )
  download.file(
    url = url,
    destfile = destfile,
    method = "curl"
  )
  # Does R have a function to get file type?
  ext <- switch(system(paste0("file -b '", destfile, "' | cut -f1 -d ' '"), intern = TRUE),
    "JPEG" = ".jpg",
    "PNG" = ".png",
    "HTML" = ".html",
    {
      cat("Unknown image type", ext, "\n")
      ""
    }
  )
  file.rename(destfile, paste0(destfile, ext))
}
image_download_logo_updated <- function(dat, image, name) {
  path <- here("data/raw/2022/winred/logo")
  if (!dir.exists(path)) dir.create(path)

  for (i in 1:nrow(dat)) {
    download_with_extension(path, dat[[image]][[i]], dat[[name]][[i]], "_logo_")
    Sys.sleep(3)
  }
}

image_download_bgimg_updated <- function(dat, image, name) {
  path <- here("data/raw/2022/winred/bgimg")
  if (!dir.exists(path)) dir.create(path)
  for (i in 1:nrow(dat)) {
    download_with_extension(path, dat[[image]][[i]], dat[[name]][[i]], "_bgimg_")
    Sys.sleep(3)
  }
}
download_image_to_dir <- function(dat, path, image, name, identifier) {
  if (!dir.exists(path)) dir.create(path)
  for (i in seq_len(nrow(dat))) {
    download_with_extension(path, dat[[image]][[i]], dat[[name]][[i]], identifier)
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


validate_image_dir <- function(path, extension) {
  stopifnot(
    "Path must be a directory" = dir.exists(path),
    "Directory must contain at least one image with given extension" = length(imgs <- Sys.glob(file.path(path, paste0("*", extension)))) > 0
  )
  imgs
}

images_load <- function(path = here::here("data", "raw", "2022", "winred", "bgimg"),
                        extension = c(".png", ".jpg")) {
  extension <- match.arg(extension)
  imgs <- validate_image_dir(path, extension)
  funs <- list(
    "png" = png::readPNG,
    "jpg" = jpeg::readJPEG
  )
  if (any(is.na(funs <- funs[tools::file_ext(imgs)]))) {
    stop("Unknown file extension")
  }
  mapply(rlang::exec, funs, imgs)
}

# Untested because unused
png2jpeg <- function(path, out_dir = path) {
  imgs <- validate_image_dir(path, ".png")
  if (!dir.exists(out_dir)) {
    dir.create(out_dir)
  }
  targets <- file.path(out_dir, tools::file_path_sans_ext(imgs), ".jpg")
  for (i in seq_along(imgs)) {
    jpeg::writeJPEG(imgs[i], target = targets[i], quality = 1)
  }
}

actblue_wrangle <- function(input) {
  ## Unused parts ==============================================================
  js_rest <- input$js_rest
  post_donation_upsells <- input$post_donation_upsells %>% bind_rows()

  ## Federal candidates ========================================================
  df <- input %>%
    select(
      -js_rest,
      -matches("^eligibility_values_es$"),
      -matches("^post_donation_upsells$")
    ) %>%
    bind_cols(., js_rest) %>%
    clean_names() %>%
    rename(id_main = id, kind_main = kind)

  if (!is.null(unlist(df$entities))) {
    df <- df %>%
      unnest(cols = c(entities), names_repair = "unique") %>%
      filter(
        ## https://www.secure.actblue.com/donate/moultonlf2019
        (kind == "candidate" | kind == "jointcommittee") &
          `federal?` == TRUE
      )
  } else {
    df$entities <- NULL
  }

  df <- df %>%
    clean_names() %>%
    dedup() %>%
    select(
      -contains("displayname_with_article"),
      -contains("contribution_blurb_bottom"),
      -contains("eligibility_values"),
      -contains("_es$"),
      -contains("disclaimer_flags"),
      -contains("radio_amounts")
    )

  if ("managing_entity" %in% names(df)) {
    df <- df %>%
      rowwise() %>%
      filter(!is.null(managing_entity)) %>%
      mutate(
        managing_id = managing_entity$id,
        managing_name = managing_entity$display_name
      ) %>%
      ungroup()
  }

  if ("display_name" %in% names(df) & "managing_name" %in% names(df)) {
    df <- df %>%
      mutate(name_diff = stringdist(display_name, managing_name))
  }

  df <- df %>%
    ungroup() %>%
    select(
      url, matches("^id$"), matches("^display_name$"),
      matches("^managing_id$"), matches("^managing_name$"),
      matches("^display_pretty_location$"), everything()
    ) %>%
    ## name_diff is 5 or more
    select(-matches("^name_diff$")) %>%
    select(-matches("^managing_entity$"))

  if ("id" %in% names(df)) {
    df <- df %>%
      rename(id_x = id, display_name_x = display_name)
  }

  if (!is.null(unlist(df$brandings))) {
    df <- df %>%
      unnest(cols = c(brandings), names_repair = "unique") %>%
      clean_names()
  }

  if ("id_x" %in% names(df) & "id" %in% names(df)) {
    df <- df %>%
      rename(
        id_brandings = id, id = id_x, display_name = display_name_x,
        display_name_brandings = display_name
      )
  } else if (("id_x" %in% names(df) & !("id" %in% names(df)))) {
    df <- df %>%
      rename(id = id_x)
  }

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
  if ("display_name" %in% names(temp)) {
    temp <- temp %>%
      rowwise() %>%
      mutate(
        ## Using case_when results in type conflicts
        display_name = ifelse(length(display_name) == 0, NA, display_name)
      )
  } else {
    temp <- temp %>% mutate(display_name = NA)
  }

  if ("managing_name" %in% names(temp)) {
    temp <- temp %>%
      rowwise() %>%
      mutate(
        ## Using case_when results in type conflicts
        managing_name = ifelse(length(managing_name) == 0, NA, managing_name)
      )
  }

  if ("display_name" %in% names(temp) & "managing_name" %in% names(temp)) {
    temp <- temp %>%
      filter(
        identical(display_name, managing_name) |
          (is.na(display_name) & !is.na(managing_name))
      )
  }

  temp <- temp %>% ungroup()

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
    select(
      -matches("^federal$"), -political,
      -matches("^charity$"), -matches("^c4$"),
      -matches("^kind$"), -matches("^managing_name$")
    )

  ## More cleaning, including amounts converted to dollar ======================
  actblue_federal <- actblue_federal %>%
    clean_names()

  if ("contribution_limit" %in% names(actblue_federal)) {
    actblue_federal <- actblue_federal %>%
      mutate(contribution_limit = contribution_limit / 100)
  }

  actblue_federal <- actblue_federal %>%
    select(
      -matches("^background_image_url$"),
      -matches("^header_image_url$"),
      -matches("^acceptable_card_types$"),
      -matches("^ticket_types$"),
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
    ) %>%
    ungroup() %>%
    dedup() %>%
    ## Delete, for now, the list data types
    select(
      -matches("^fundraising_video$"),
      -matches("^relevant_surrogate_keys$"),
      -matches("^share_content$"),
      -matches("^list_disclaimer_policy$"),
      -matches("^analytics_trackers_attributes$"),
      -matches("^recurring_promotions$"),
      -matches("^formatted_variations_hash$"),
      -matches("^ab_test$"),
      -matches("^custom_fields$"),
      -matches("^acceptable_card_types$")
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
      title,
      text = contribution_blurb
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
    mutate(text = paste(text, og_description, sep = " \n ")) %>%
    select(
      display_name = description, url = og_url, fec_id_cand = FEC_ID_cand,
      first_name, last_name, title = og_title, text
    )

  ## Note that unlike ActBlue, WinRed same-link *did* change content!!
  ## https://secure.winred.com/marco-rubio-for-senate/website
}

ngp_select_text <- function(url) {
  ## Setting variables to NA
  landing_text <- meta_data <- landing_logo <- landing_footer <-
    contribution_amounts <- NA
  ## Starting rselenium
  rs <- rsDriver(browser = "firefox", port = netstat::free_port())
  remote <- rs$client
  remote$navigate(url)

  ## Metadata
  meta_data <- read_html(remote$getPageSource()[[1]]) %>% {
    tibble(
      name = html_attr(., "name"),
      property = html_attr(., "property"),
      content = html_attr(., "content")
    )
  }

  ## Landing page text
  landing_text <- read_html(remote$getPageSource()[[1]]) %>%
    html_nodes(xpath = '//*[@class="at-markup HeaderHtml"]') %>%
    html_text()
  landing_text <- gsub("\n", "", gsub("\t", "", landing_text)) %>%
    trimws()
  if (length(landing_text) == 0) landing_text <- ""

  ## Landing page logo
  landing_logo <- read_html(remote$getPageSource()[[1]]) %>%
    html_nodes(xpath = '//*[@class="at-banner"]') %>%
    html_nodes("img") %>%
    html_attr("src")
  if (length(landing_logo) == 0) landing_logo <- ""

  ## Landing page footer
  landing_footer <- read_html(remote$getPageSource()[[1]]) %>%
    html_nodes(xpath = '//*[@class="at-markup FooterHtml clearfix"]') %>%
    html_text()
  landing_footer <- gsub("\n", "", landing_footer) %>%
    trimws()
  if (length(landing_footer) == 0) landing_footer <- ""

  ## Contribution amounts
  contribution_amounts <- read_html(remote$getPageSource()[[1]]) %>%
    html_nodes(xpath = '//*[@class="at-radio"]') %>%
    html_nodes("label") %>%
    html_attr("title") %>%
    map(~ gsub("\n", "", .x)) %>%
    unlist() %>%
    toString()

  ## Preparing output
  out <- meta_data %>%
    mutate(name = coalesce(name, property)) %>%
    filter(!is.na(name)) %>%
    select(-property) %>%
    pivot_wider(values_from = content) %>%
    janitor::clean_names()

  out <- tibble(url = url, date = Sys.Date())
  out <- out %>%
    mutate(
      amounts = contribution_amounts,
      text = landing_text,
      footer = landing_footer,
      logo = landing_logo
    )

  remote$close()
  rs[["server"]]$stop()
  rm(rs)
  gc(reset = TRUE)

  return(out)
}

raisethemoney_select_text <- function(url) {
  ## Setting variables to NA
  landing_text <- meta_data <- landing_footer <-
    contribution_amounts <- NA
  ## Starting rselenium
  rs <- rsDriver(browser = "firefox", port = netstat::free_port())
  remote <- rs$client
  remote$navigate(url)

  ## Metadata
  meta_data <- read_html(remote$getPageSource()[[1]]) %>% {
    tibble(
      name = html_attr(., "name"),
      property = html_attr(., "property"),
      content = html_attr(., "content")
    )
  }

  ## Landing page text
  landing_text <- read_html(remote$getPageSource()[[1]]) %>%
    html_nodes(xpath = '//*[@class="paper_new"]') %>%
    html_text()
  landing_text <- gsub("\n", "", landing_text) %>%
    trimws()
  if (length(landing_text) == 0) landing_text <- ""

  ## Landing page footer
  landing_footer <- read_html(remote$getPageSource()[[1]]) %>%
    html_nodes(xpath = '//*[@class="space-top-small markdown contribution-terms text-black"]') %>%
    html_text()
  landing_footer <- gsub("\n", "", landing_footer) %>%
    trimws()
  if (length(landing_footer) == 0) landing_footer <- ""

  ## Contribution amounts
  contribution_amounts <- read_html(remote$getPageSource()[[1]]) %>%
    html_nodes(xpath = '//*[@class="contribution-amount"]') %>%
    html_nodes("label") %>%
    html_text() %>%
    map(~ gsub("\n", "", .x)) %>%
    unlist() %>%
    toString()

  ## Preparing output
  out <- meta_data %>%
    mutate(name = coalesce(name, property)) %>%
    filter(!is.na(name)) %>%
    select(-property) %>%
    pivot_wider(values_from = content) %>%
    janitor::clean_names()

  out <- tibble(url = url, date = Sys.Date())
  out <- out %>%
    mutate(
      amounts = contribution_amounts,
      text = landing_text,
      footer = landing_footer
    )

  remote$close()
  rs[["server"]]$stop()
  rm(rs)
  gc(reset = TRUE)

  return(out)
}

clickandpledge_select_text <- function(url) {
  ## Setting variables to NA
  meta_data <- contribution_amounts <- NA
  ## Starting rselenium
  rs <- rsDriver(browser = "firefox", port = netstat::free_port())
  remote <- rs$client
  remote$navigate(url)

  ## Metadata
  meta_data <- read_html(remote$getPageSource()[[1]]) %>% {
    tibble(
      name = html_attr(., "name"),
      property = html_attr(., "property"),
      content = html_attr(., "content")
    )
  }

  ## Contribution amounts
  contribution_amounts <- read_html(remote$getPageSource()[[1]]) %>%
    html_nodes(xpath = '//*[@class="amount-buttons row margin-top-10"]') %>%
    html_nodes("button") %>%
    html_text() %>%
    map(~ gsub("\n", "", .x))
  contribution_amounts <- contribution_amounts[c(1:5)] %>%
    unlist() %>%
    toString()

  ## Preparing output
  out <- meta_data %>%
    mutate(name = coalesce(name, property)) %>%
    filter(!is.na(name)) %>%
    select(-property) %>%
    pivot_wider(values_from = content) %>%
    janitor::clean_names()

  out <- tibble(url = url, date = Sys.Date())
  out <- out %>%
    mutate(
      amounts = contribution_amounts
    )

  remote$close()
  rs[["server"]]$stop()
  rm(rs)
  gc(reset = TRUE)

  return(out)
}

transaxt_select_text <- function(url) {
  ## Setting variables to NA
  meta_data <- landing_footer <- contribution_amounts <- NA
  ## Starting rselenium
  rs <- rsDriver(browser = "firefox", port = netstat::free_port())
  remote <- rs$client
  remote$navigate(url)

  ## Metadata
  meta_data <- read_html(remote$getPageSource()[[1]]) %>% {
    tibble(
      name = html_attr(., "name"),
      property = html_attr(., "property"),
      content = html_attr(., "content")
    )
  }

  ## Landing page footer
  landing_footer <- read_html(remote$getPageSource()[[1]]) %>%
    html_nodes(xpath = '//*[@class="compliance legal-text"]') %>%
    html_text()
  landing_footer <- gsub("\n", "", landing_footer) %>%
    trimws()
  if (length(landing_footer) == 0) landing_footer <- ""

  ## Contribution amounts
  contribution_amounts <- read_html(remote$getPageSource()[[1]]) %>%
    html_nodes(xpath = '//*[@class="control"]') %>%
    html_nodes("label") %>%
    html_text() %>%
    map(~ gsub("\n", "", .x)) %>%
    unlist() %>%
    toString()

  ## Preparing output
  out <- meta_data %>%
    mutate(name = coalesce(name, property)) %>%
    filter(!is.na(name)) %>%
    select(-property) %>%
    pivot_wider(values_from = content) %>%
    janitor::clean_names()

  out <- tibble(url = url, date = Sys.Date())
  out <- out %>%
    mutate(
      amounts = contribution_amounts,
      footer = landing_footer
    )

  remote$close()
  rs[["server"]]$stop()
  rm(rs)
  gc(reset = TRUE)

  return(out)
}

donorbox_select_text <- function(url) {
  ## Setting variables to NA
  landing_text_title <- landing_text_description <- landing_text <- meta_data <-
    landing_logo <- contribution_amounts <- NA
  ## Starting rselenium
  rs <- rsDriver(browser = "firefox", port = netstat::free_port())
  remote <- rs$client
  remote$navigate(url)

  ## Metadata
  meta_data <- read_html(remote$getPageSource()[[1]]) %>% {
    tibble(
      name = html_attr(., "name"),
      property = html_attr(., "property"),
      content = html_attr(., "content")
    )
  }

  ## Landing page text
  landing_text_title <- read_html(remote$getPageSource()[[1]]) %>%
    html_nodes(xpath = '//*[@class="columns campaign-columns"]') %>%
    html_text()
  landing_text_description <- read_html(remote$getPageSource()[[1]]) %>%
    html_nodes(xpath = '//*[@class="donor-campaign-details fr-view"]') %>%
    html_text()
  landing_text <- paste(landing_text_title, landing_text_description)
  landing_text <- gsub("\n", "", gsub("\t", "", landing_text)) %>%
    trimws()
  if (length(landing_text) == 0) landing_text <- ""

  ## Landing page logo
  landing_logo <- read_html(remote$getPageSource()[[1]]) %>%
    html_nodes(xpath = '//*[@class="org-logo"]') %>%
    html_attr("src")
  if (length(landing_logo) == 0) landing_logo <- ""

  ## Contribution amounts
  contribution_amounts <- read_html(remote$getPageSource()[[1]]) %>%
    html_nodes(xpath = '//*[@class="amount-item suggested-amount-item"]') %>%
    html_nodes(xpath = '//*[@class="amount"]') %>%
    html_text() %>%
    map(~ gsub("\n", "", .x)) %>%
    unlist() %>%
    toString()

  ## Preparing output
  out <- meta_data %>%
    mutate(name = coalesce(name, property)) %>%
    filter(!is.na(name)) %>%
    select(-property) %>%
    pivot_wider(values_from = content) %>%
    janitor::clean_names()

  out <- tibble(url = url, date = Sys.Date())
  out <- out %>%
    mutate(
      amounts = contribution_amounts,
      text = landing_text,
      logo = landing_logo
    )

  remote$close()
  rs[["server"]]$stop()
  rm(rs)
  gc(reset = TRUE)

  return(out)
}

authorize_select_text <- function(url) {
  ## Setting variables to NA
  landing_text <- meta_data <- landing_footer_compliance <-
    landing_footer_note <- landing_footer <- NA
  ## Starting rselenium
  rs <- rsDriver(browser = "firefox", port = netstat::free_port())
  remote <- rs$client
  remote$navigate(url)

  ## Metadata
  meta_data <- read_html(remote$getPageSource()[[1]]) %>% {
    tibble(
      name = html_attr(., "name"),
      property = html_attr(., "property"),
      content = html_attr(., "content")
    )
  }

  ## Landing page text
  landing_text <- read_html(remote$getPageSource()[[1]]) %>%
    html_nodes(xpath = '//*[@id="spnMerchantHTMLHeader"]') %>%
    html_text()
  landing_text <- gsub("\n", "", gsub("\t", "", landing_text)) %>%
    trimws()
  if (length(landing_text) == 0) landing_text <- ""

  ## Landing page footer
  landing_footer_compliance <- read_html(remote$getPageSource()[[1]]) %>%
    html_nodes(xpath = '//*[@id="spnItemDescription3"]') %>%
    html_text()
  landing_footer_note <- read_html(remote$getPageSource()[[1]]) %>%
    html_nodes(xpath = '//*[@id="spnMerchantHTMLFooter"]') %>%
    html_text()
  landing_footer <- paste(landing_footer_compliance, landing_footer_note)
  landing_footer <- gsub("\n", "", landing_footer) %>%
    trimws()
  if (length(landing_footer) == 0) landing_footer <- ""

  ## Preparing output
  out <- meta_data %>%
    mutate(name = coalesce(name, property)) %>%
    filter(!is.na(name)) %>%
    select(-property) %>%
    pivot_wider(values_from = content) %>%
    janitor::clean_names()

  out <- tibble(url = url, date = Sys.Date())
  out <- out %>%
    mutate(
      text = landing_text,
      footer = landing_footer
    )

  remote$close()
  rs[["server"]]$stop()
  rm(rs)
  gc(reset = TRUE)

  return(out)
}

piryx_select_text <- function(url) {
  ## Setting variables to NA
  landing_text <- meta_data <- landing_logo <- landing_footer <-
    contribution_amounts <- NA
  ## Starting rselenium
  rs <- rsDriver(browser = "firefox", port = netstat::free_port())
  remote <- rs$client
  remote$navigate(url)

  ## Metadata
  meta_data <- read_html(remote$getPageSource()[[1]]) %>% {
    tibble(
      name = html_attr(., "name"),
      property = html_attr(., "property"),
      content = html_attr(., "content")
    )
  }

  ## Landing page text
  landing_text <- read_html(remote$getPageSource()[[1]]) %>%
    html_nodes(xpath = '//*[@id="page"]') %>%
    html_nodes(xpath = '//*[@id="heading"]') %>%
    html_text()
  landing_text <- gsub("\n", "", gsub("\t", "", landing_text)) %>%
    trimws()
  if (length(landing_text) == 0) landing_text <- ""

  ## Landing page logo
  landing_logo <- read_html(remote$getPageSource()[[1]]) %>%
    html_nodes(xpath = '//*[@id="logo"]') %>%
    html_nodes("img") %>%
    html_attr("src")
  if (length(landing_logo) == 0) landing_logo <- ""

  ## Landing page footer
  landing_footer <- read_html(remote$getPageSource()[[1]]) %>%
    html_nodes(xpath = '//*[@id="legal-compliance"]') %>%
    html_text()
  landing_footer <- gsub("\n", "", landing_footer) %>%
    trimws()
  if (length(landing_footer) == 0) landing_footer <- ""

  ## Contribution amounts
  contribution_amounts <- read_html(remote$getPageSource()[[1]]) %>%
    html_nodes(xpath = '//*[@class="piryx-donation-form"]') %>%
    html_nodes(xpath = '//*[@id="amount"]') %>%
    html_nodes(xpath = '//*[@class="amount"]') %>%
    html_text() %>%
    map(~ gsub("\n", "", .x)) %>%
    unlist() %>%
    toString()

  ## Preparing output
  out <- meta_data %>%
    mutate(name = coalesce(name, property)) %>%
    filter(!is.na(name)) %>%
    select(-property) %>%
    pivot_wider(values_from = content) %>%
    janitor::clean_names()

  out <- tibble(url = url, date = Sys.Date())
  out <- out %>%
    mutate(
      text = landing_text,
      footer = landing_footer,
      amounts = contribution_amounts,
      logo = landing_logo
    )

  remote$close()
  rs[["server"]]$stop()
  rm(rs)
  gc(reset = TRUE)

  return(out)
}

numero_select_text <- function(url) {
  ## Setting variables to NA
  landing_text <- meta_data <- landing_logo <- landing_footer <-
    contribution_amounts <- NA
  ## Starting rselenium
  rs <- rsDriver(browser = "firefox", port = netstat::free_port())
  remote <- rs$client
  remote$navigate(url)

  ## Metadata
  meta_data <- read_html(remote$getPageSource()[[1]]) %>% {
    tibble(
      name = html_attr(., "name"),
      property = html_attr(., "property"),
      content = html_attr(., "content")
    )
  }

  ## Landing page text
  landing_text <- read_html(remote$getPageSource()[[1]]) %>%
    html_nodes(xpath = '//*[@class="col-md-5"]') %>%
    html_text()
  landing_text <- gsub("\n", "", gsub("\t", "", landing_text)) %>%
    trimws()
  if (length(landing_text) == 0) landing_text <- ""

  ## Landing page logo
  landing_logo <- read_html(remote$getPageSource()[[1]]) %>%
    html_nodes(xpath = '//*[@class="col-xl-3 col-md-5 col-sm-7 col-8"]') %>%
    html_nodes("img") %>%
    html_attr("src")
  if (length(landing_logo) == 0) landing_logo <- ""

  ## Landing page footer
  landing_footer <- read_html(remote$getPageSource()[[1]]) %>%
    html_nodes(xpath = '//*[@class="pt-1 card-body"]') %>%
    html_text()
  landing_footer <- gsub("\n", "", landing_footer) %>%
    trimws()
  if (length(landing_footer) == 0) landing_footer <- ""

  ## Contribution amounts
  contribution_amounts <- read_html(remote$getPageSource()[[1]]) %>%
    html_nodes(xpath = '//*[@class="card-body"]') %>%
    html_nodes("button") %>%
    html_text() %>%
    map(~ gsub("\n", "", .x)) %>%
    unlist() %>%
    toString()

  ## Preparing output
  out <- meta_data %>%
    mutate(name = coalesce(name, property)) %>%
    filter(!is.na(name)) %>%
    select(-property) %>%
    pivot_wider(values_from = content) %>%
    janitor::clean_names()

  out <- tibble(url = url, date = Sys.Date())
  out <- out %>%
    mutate(
      text = landing_text,
      footer = landing_footer,
      amounts = contribution_amounts,
      logo = landing_logo
    )

  remote$close()
  rs[["server"]]$stop()
  rm(rs)
  gc(reset = TRUE)

  return(out)
}

efundraising_select_text <- function(url) {
  ## Setting variables to NA
  landing_text <- meta_data <- landing_logo <- landing_footer <-
    contribution_amounts <- NA
  ## Starting rselenium
  rs <- rsDriver(browser = "firefox", port = netstat::free_port())
  remote <- rs$client
  remote$navigate(url)

  ## Metadata
  meta_data <- read_html(remote$getPageSource()[[1]]) %>% {
    tibble(
      name = html_attr(., "name"),
      property = html_attr(., "property"),
      content = html_attr(., "content")
    )
  }

  ## Landing page text
  landing_text <- read_html(remote$getPageSource()[[1]]) %>%
    html_nodes(xpath = '//*[@class="efund_title"]') %>%
    html_text()
  landing_text <- gsub("\n", "", gsub("\t", "", landing_text)) %>%
    trimws()
  if (length(landing_text) == 0) landing_text <- ""

  ## Landing page logo
  landing_logo <- read_html(remote$getPageSource()[[1]]) %>%
    html_nodes(xpath = '//*[@class="efund_title"]') %>%
    html_nodes("img") %>%
    html_attr("src")
  if (length(landing_logo) == 0) landing_logo <- ""

  ## Landing page footer
  landing_footer <- read_html(remote$getPageSource()[[1]]) %>%
    html_nodes(
      xpath =
        '//*[@class="efund-form-col col-sm-12 col-md-10 col-lg-8 col-xl-6"]'
    ) %>%
    html_text()
  landing_footer <- gsub("\n", "", landing_footer) %>%
    trimws()
  if (length(landing_footer) == 0) landing_footer <- ""

  ## Contribution amounts
  contribution_amounts <- read_html(remote$getPageSource()[[1]]) %>%
    html_nodes(xpath = '//*[@class="card-body"]') %>%
    html_nodes(xpath = '//*[@class="efund_PaymentOptionItem"]') %>%
    html_attr("data-amount") %>%
    map(~ gsub("\n", "", .x)) %>%
    unlist() %>%
    toString()

  ## Preparing output
  out <- meta_data %>%
    mutate(name = coalesce(name, property)) %>%
    filter(!is.na(name)) %>%
    select(-property) %>%
    pivot_wider(values_from = content) %>%
    janitor::clean_names()

  out <- tibble(url = url, date = Sys.Date())
  out <- out %>%
    mutate(
      text = landing_text,
      footer = landing_footer,
      amounts = contribution_amounts,
      logo = landing_logo
    )

  remote$close()
  rs[["server"]]$stop()
  rm(rs)
  gc(reset = TRUE)

  return(out)
}

fundhero_select_text <- function(url) {
  ## Setting variables to NA
  landing_text <- meta_data <- landing_logo <- landing_footer <-
    contribution_amounts <- NA
  ## Starting rselenium
  rs <- rsDriver(browser = "firefox", port = netstat::free_port())
  remote <- rs$client
  remote$navigate(url)

  ## Metadata
  meta_data <- read_html(remote$getPageSource()[[1]]) %>% {
    tibble(
      name = html_attr(., "name"),
      property = html_attr(., "property"),
      content = html_attr(., "content")
    )
  }

  ## Landing page text
  landing_text <- read_html(remote$getPageSource()[[1]]) %>%
    html_nodes(xpath = '//*[@class="p20"]') %>%
    html_nodes("h1") %>%
    html_text()
  landing_text <- gsub("\n", "", gsub("\t", "", landing_text)) %>%
    trimws()
  if (length(landing_text) == 0) landing_text <- ""

  ## Landing page logo
  landing_logo <- read_html(remote$getPageSource()[[1]]) %>%
    html_nodes(xpath = '//*[@class="img-wrapper"]') %>%
    html_nodes(xpath = '//*[@class="hero img-child"]') %>%
    html_attr("style")
  landing_logo <- landing_logo[1]

  url_pattern <- paste0(
    "http[s]?://(?:[a-zA-Z]|[0-9]|[$-_@.&+]|",
    "[!*\\(\\),]|(?:%[0-9a-fA-F][0-9a-fA-F]))+"
  )

  landing_logo <- str_extract(landing_logo, url_pattern)
  landing_logo <- sub("\\).*", "", landing_logo)
  if (length(landing_logo) == 0) landing_logo <- ""

  ## Landing page footer
  landing_footer <- read_html(remote$getPageSource()[[1]]) %>%
    html_nodes(xpath = '//*[@class="middle2"]') %>%
    html_text()
  landing_footer <- gsub("\n", "", landing_footer) %>%
    trimws()
  if (length(landing_footer) == 0) landing_footer <- ""

  ## Contribution amounts
  contribution_amounts <- read_html(remote$getPageSource()[[1]]) %>%
    html_nodes(xpath = '//*[@class="buttons-container"]') %>%
    html_nodes(xpath = '//*[@class="g-btn"]') %>%
    map(~ gsub("\n", "", .x)) %>%
    unlist() %>%
    toString()

  ## Preparing output
  out <- meta_data %>%
    mutate(name = coalesce(name, property)) %>%
    filter(!is.na(name)) %>%
    select(-property) %>%
    pivot_wider(values_from = content) %>%
    janitor::clean_names()

  out <- tibble(url = url, date = Sys.Date())
  out <- out %>%
    mutate(
      text = landing_text,
      footer = landing_footer,
      amounts = contribution_amounts,
      logo = landing_logo
    )

  remote$close()
  rs[["server"]]$stop()
  rm(rs)
  gc(reset = TRUE)

  return(out)
}

ngpvan_select_text <- function(url) {
  ## secure.ngpvan.com
  out <- httr::GET(url)
  out <- httr::content(out)

  designation <- out$designation
  out$designation <- NULL
  out$form_elements <- NULL ## too many, skip
  out$metadata <- NULL ## similarly unnecessary
  out$url <- NULL

  out <- out %>%
    imap(~ tibble(!!as.name(.y) := .x)) %>%
    bind_cols() %>%
    clean_names()

  if (!is.null(designation)) {
    designation <- designation %>%
      imap(~ tibble(!!as.name(.y) := .x)) %>%
      bind_cols() %>%
      clean_names() %>%
      rename(designation_name = name)

    out <- out %>%
      bind_cols(., designation)
  }

  return(out)
}

minor_platforms <- function(cong, platform) {
  cong %>%
    imap(
      ~ {
        df <- .x %>% filter(grepl(platform, url))
        if (nrow(df) > 1) {
          out <- vector(mode = "list", length = nrow(df))
          fp <- here("data", "raw", "wayback", .y, "html")

          for (x in seq(nrow(df))) {
            tryCatch(
              {
                out[[x]] <-
                  get(eval(paste0(platform, "_select_text")))((df$link[x])) %>%
                  select(-matches("^date$"))

                if ("url" %in% names(out[[x]])) {
                  out[[x]] <- out[[x]] %>%
                    rename(link = url)
                }

                out[[x]] <- out[[x]] %>%
                  bind_cols(., df[x, c("url")]) %>%
                  select(url, -matches("^link$"), everything())
              },
              error = function(e) {
                message(e)
              }
            )

            if ((x %% 50 == 0) | x == nrow(df)) {
              save(
                out,
                file = here(
                  "data/raw",
                  paste0(platform, "_text_", .y, "_list.Rda")
                )
              )
            }
          }
        }
      }
    )
}

save_text_df <- function(platform) {
  c(senate = "senate", house = "house") %>%
    map(
      ~ loadRData(
        here("data", "raw", paste0(platform, "_text_", .x, "_list.Rda"))
      ) %>%
        keep(~ !is.null(.x)) %>%
        map(clean_names) %>%
        bind_rows() %>%
        clean_names()
    )
}

fb_short <- function(id, token,
                     ## https://www.facebook.com/ads/library/api/releasenotes
                     ## Still v12.0 for now: use ?adlib_build_query
                     ## Full fields give error:
                     ## Please reduce the amount of data you're asking for,
                     ## then retry your request
                     fields,
                     # fields = c(
                     #   "ad_creation_time",
                     #   "ad_creative_body",
                     #   "ad_creative_link_caption",
                     #   "ad_creative_link_description",
                     #   "ad_creative_link_title",
                     #   "ad_delivery_start_time",
                     #   "ad_delivery_stop_time",
                     #   "ad_snapshot_url",
                     #   "currency",
                     #   "demographic_distribution",
                     #   "funding_entity",
                     #   "impressions",
                     #   "page_id",
                     #   "page_name",
                     #   "potential_reach",
                     #   "publisher_platforms",
                     #   "region_distribution",
                     #   "spend"
                     # ),
                     max_date = "2020-12-31",
                     min_date = "2019-01-01",
                     limit = 5000,
                     ...) {
  if (!(fields %in% c("ad_data", "demographic_data", "region_data"))) {
    stop("Use the specified set of columns for now.")
  }

  query <- adlib_build_query(
    ad_reached_countries = "US",
    ad_active_status = "ALL",
    ## ad_type = "POLITICAL_AND_ISSUE_ADS", ---> this is implicit
    search_page_ids = id,
    ad_delivery_date_max = max_date,
    ad_delivery_date_min = min_date,
    limit = limit,
    fields = fields,
    ...
  )
  resp <- adlib_get(params = query, token = token)

  if (fields == "ad_data") {
    if (length(resp$data) > 0) {
      out <- as_tibble(resp, type = "ad")
    } else {
      out <- NULL
    }
  } else if (fields == "demographic_data") {
    ## later, perform
    ## unnest(out, cols = c(demographic_distribution))
    out <- parse_response(resp, list_cols = "demographic_distribution")
  } else if (fields == "region_data") {
    out <- parse_response(resp, list_cols = "region_distribution")
  }

  ## Return all query, response object itself, and tibble
  return(list(query = query, resp = resp, tbl = out))
}

simplify_ad_body <- function(df) {
  df %>%
    mutate(
      ad_creative_link_caption = gsub(
        "/es$|^www\\.|/$|^https://|^https://www.",
        "", ad_creative_link_caption
      )
    )
}

tally_by_cand <- function(x, lim, party = TRUE) {
  if (party) {
    x %>%
      group_by(candidate, party) %>%
      tally() %>%
      group_by(party) %>%
      arrange(-n) %>%
      slice(seq(lim))
  } else {
    x %>%
      group_by(candidate) %>%
      tally() %>%
      arrange(-n) %>%
      slice(seq(lim))
  }
}

top_freq <- function(l1, l2, var, party = TRUE) {
  if (party) {
    out <- c(senate = "senate", house = "house") %>%
      imap(
        ~ l1[[.x]] %>%
          group_by(candidate, !!as.name(var), party)
      )
  } else {
    out <- c(senate = "senate", house = "house") %>%
      imap(
        ~ l1[[.x]] %>%
          group_by(candidate, !!as.name(var))
      )
  }
  out <- out %>%
    imap(
      ~ .x %>%
        tally() %>%
        filter(candidate %in% l2[[.y]]$candidate) %>%
        ungroup() %>%
        complete(candidate, !!as.name(var), fill = list(n = 0)) %>%
        group_by(candidate) %>%
        mutate(party = Mode(party))
    )
}

top_freq_plot <- function(x, chamber, top, title = NULL, subtitle = NULL,
                          party = TRUE, save = TRUE,
                          width = 7, height = 3.5, fxn = "bar", ...) {
  xlab <- ifelse(fxn == "bar", "Number of Ads", "Percentage of Ads")
  p <- x$freq[[chamber]] %>%
    mutate(
      m = case_when(
        !!as.name(x$var) == 0 ~ x$lab0,
        !!as.name(x$var) == 1 ~ x$lab1
      )
    )

  if (fxn == "bar") {
    p <- p %>%
      ggplot(aes(x = n, y = fct_reorder(candidate, total), fill = fct_rev(m))) +
      geom_col() +
      scale_x_continuous(labels = scales::comma)
  } else {
    p <- p %>%
      ggplot(
        aes(x = perc, y = fct_reorder(candidate, perc1), fill = fct_rev(m))
      ) +
      geom_col() +
      scale_x_continuous(labels = scales::percent)
  }

  p <- p +
    ## scale_fill_brewer(type = "qual", palette = 6) +
    scale_fill_manual(values = c("red", "gray"))

  if (!is.null(title)) {
    p <- p + labs(title = title)
  }
  if (!is.null(subtitle)) {
    p <- p + labs(subtitle = subtitle)
  }
  if (party) {
    p <- p + facet_wrap(~party, ...)
  }

  p <- pdf_default(p) +
    theme(
      legend.position = "bottom",
      legend.justification = "left",
      legend.title = element_blank(),
      axis.title.y = element_blank()
    ) +
    xlab(xlab)

  if (top > 30) {
    p <- p +
      theme(
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()
      )
  }

  if (save) {
    pdf(
      here(
        "fig",
        paste0(x$var, "_", chamber, "_", x$type, "_top_", top, "_", fxn, ".pdf")
      ),
      width = width, height = height
    )
    print(p)
    dev.off()
  }

  return(p)
}

color4 <- c(
  "Republican,\nDonor-targeting" = "#ca0020", ## dark red
  "Republican,\nVoter-targeting" = "#f4a582", ## light red
  "Democrat,\nDonor-targeting" = "#0571b0", ## dark blue
  "Democrat,\nVoter-targeting" = "#92c5de" ## light blue
)

color4_platform <- c(
  "WinRed" = "#ca0020", ## dark red
  "Other Rep.\nPlatform" = "#f4a582", ## light red
  "ActBlue" = "#0571b0", ## dark blue
  "Other Dem.\nPlatform" = "#92c5de" ## light blue
)

fb_perspective_plot <- function(df, xvar, se, xlab, full = FALSE) {
  p <- df %>%
    ggplot(
      aes(
        y = fct_rev(type), x = !!as.name(xvar),
        fill = type, color = type, shape = financial,
        xmax = !!as.name(xvar) + 1.96 * !!as.name(se),
        xmin = !!as.name(xvar) - 1.96 * !!as.name(se)
      )
    ) +
    geom_col(width = .5) +
    geom_errorbar(width = .2, size = 1, aes(color = "black")) +
    facet_wrap(~chamber) +
    scale_color_manual(values = color4) +
    scale_fill_manual(values = color4) +
    labs(y = "", x = xlab)

  if (full) {
    p <- p +
      scale_color_manual(values = color4_platform) +
      scale_fill_manual(values = color4_platform)
    pdf_default(p) +
      theme(legend.position = "none") +
      scale_x_continuous(limits = c(0, 0.14))
  } else {
    pdf_default(p) +
      theme(legend.position = "none") +
      scale_x_continuous(limits = c(0, 0.14))
  }
}

fb_mention_plot <- function(df, xvar, se, xlab, full = FALSE) {
  p <- df %>%
    ggplot(
      aes(
        y = fct_rev(Party), x = !!as.name(xvar),
        fill = fct_rev(Party), color = fct_rev(Party),
        xmax = !!as.name(xvar) + 1.96 * !!as.name(se),
        xmin = !!as.name(xvar) - 1.96 * !!as.name(se)
      )
    ) +
    geom_col(width = .5) +
    geom_errorbar(width = .2, size = 1, aes(color = "black")) +
    facet_wrap(~chamber) +
    scale_fill_manual(values = color4) +
    scale_color_manual(values = color4) +
    labs(y = "", x = xlab)

  if (full) {
    p <- p +
      scale_color_manual(values = color4_platform)
    pdf_default(p) +
      theme(legend.position = "none") +
      scale_x_continuous(breaks = seq(0, 0.3, by = 0.05), limits = c(0, 0.3))
  } else {
    pdf_default(p) +
      theme(legend.position = "none") +
      scale_x_continuous(breaks = seq(0, 0.3, by = 0.05), limits = c(0, 0.3))
  }
}

party_factor <- function(x, outvar) {
  x %>%
    rowwise() %>%
    mutate(party = simple_cap(tolower(party))) %>%
    mutate(financial = simple_cap(tolower(financial))) %>%
    mutate(!!as.name(outvar) := glue("{party},\n{financial}")) %>%
    ungroup() %>%
    mutate(
      !!as.name(outvar) := factor(
        !!as.name(outvar),
        levels = c(
          "Republican,\nDonor-targeting", "Republican,\nVoter-targeting",
          "Democrat,\nDonor-targeting", "Democrat,\nVoter-targeting"
        )
      )
    )
}

dict_plot <- function(df, var = "troll") {
  p <- df %>%
    filter(party != "INDEPENDENT") %>%
    filter(party != "NANA") %>%
    filter(!is.na(party)) %>%
    rowwise() %>%
    mutate(chamber = simple_cap(chamber)) %>%
    mutate(financial = simple_cap(tolower(financial))) %>%
    mutate(party = simple_cap(tolower(party))) %>%
    ungroup() %>%
    party_factor(., outvar = "party") %>%
    rename(Party = party) %>%
    group_by(Party, chamber) %>%
    summarise(avg = mean(!!as.name(var))) %>%
    ggplot(aes(x = avg, y = fct_rev(Party), fill = fct_rev(Party))) +
    geom_col(width = .5) + ## alpha = .8
    facet_grid(~chamber) +
    scale_fill_manual(values = color4) +
    labs(
      y = "",
      x = "Average Number of Dictionary Words per Document"
      # title = paste0(
      #   "Occurrence of Words From the Troll-And-Incivility Dictionary",
      #   "Broken Down by Party and Chamber of Us Congress"
      # ),
      # subtitle = "Corpus: Candidate Websites"
    )
  return(pdf_default(p))
}

emotion_plot <- function(x, title) {
  p <- x %>%
    mutate(index = row_number()) %>%
    select(
      index, Dic, anger, anticipation, disgust, fear, joy, negative,
      positive, sadness, surprise, trust
    ) %>%
    mutate(sum = rowSums(across(anger:trust)) / Dic) %>%
    select(index, anger, disgust, fear, positive) %>%
    ## But what about joy and anticipation?
    ## Also, other = 100 - anger - disgust - fear - negative - positive --->
    ## misleading; oftentimes, negative == fear, etc etc.
    mutate(other = 100 - anger - disgust - fear - positive) %>%
    pivot_longer(cols = anger:other) %>%
    ggplot(
      aes(
        y = fct_reorder(factor(index), value), x = value, color = name,
        fill = name
      )
    ) +
    geom_col() +
    scale_fill_manual(
      values = c("#202547", "#53354A", "#BE3144", "#FF7844", "#E3D26F")
    ) +
    scale_color_manual(
      values = c("#202547", "#53354A", "#BE3144", "#FF7844", "#E3D26F")
    ) +
    # jcolors::scale_fill_jcolors(palette = "pal4") +
    # jcolors::scale_color_jcolors(palette = "pal4") +
    labs(
      x = "", y = "",
      ## x = "Proportion of Classified Words", y = "",
      ## title = "NRC Language Analysis of Senate Republican Statements",
      title = title,
      color = "", fill = ""
    )
  return(
    pdf_default(p) +
      theme(
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()
      ) +
      scale_x_continuous(limits = c(0, 100))
  )
}

emotion_barplot <- function(x) {
  p <- x %>%
    imap_dfr(
      ~ .x %>%
        mutate(index = row_number()) %>%
        select(
          index, Dic, anger, anticipation, disgust, fear, joy, negative,
          positive, sadness, surprise, trust
        ) %>%
        mutate(sum = rowSums(across(anger:trust)) / Dic) %>%
        select(index, anger, disgust, fear, positive) %>%
        summarise(
          anger = mean(anger), disgust = mean(disgust),
          fear = mean(fear), positive = mean(positive)
        ) %>%
        mutate(other = 100 - anger - disgust - fear - positive),
      .id = "type"
    ) %>%
    pivot_longer(cols = anger:other) %>%
    mutate(name = factor(name)) %>%
    ## alphabetically okay
    rowwise() %>%
    mutate(
      title = paste0(
        ifelse(
          grepl("d", str_match_all(type, "_(.*?)_")[[1]][1, 2]),
          "Democrat", "Republican"
        ),
        ",\n",
        ifelse(
          grepl("f", str_match_all(type, "_(.*?)$")[[1]][1, 2]),
          "Donor-targeting", "Voter-targeting"
        )
      )
    ) %>%
    ungroup() %>%
    ggplot(
      aes(y = title, x = value, color = fct_rev(name), fill = fct_rev(name))
    ) +
    geom_col(width = .5) +
    scale_fill_manual(
      values = rev(c("#202547", "#53354A", "#BE3144", "#FF7844", "#E3D26F"))
    ) +
    scale_color_manual(
      values = rev(c("#202547", "#53354A", "#BE3144", "#FF7844", "#E3D26F"))
    ) +
    labs(
      x = "", y = "",
      color = "", fill = ""
    )
  return(pdf_default(p))
}

topic_plot <- function(x, gvar = NULL, grouped = FALSE) {
  if (grouped) {
    p <- x %>%
      mutate(prop = n / sum(n))
  } else {
    p <- x %>%
      group_by(!!as.name(gvar)) %>%
      dplyr::count(!!as.name(gvar), topic) %>%
      mutate(prop = n / sum(n))
  }
  p <- p %>%
    ggplot(
      aes(x = prop * 100, y = fct_rev(!!as.name(gvar)), fill = fct_rev(topic))
    ) +
    geom_bar(stat = "identity", alpha = .7, width = .5) +
    scale_fill_brewer(type = "qual", palette = 3) +
    labs(y = "", x = "Percent of Ads", fill = "Topic")
  pdf_default(p) + theme(legend.position = "bottom")
}

summ_df_fxn <- function(df, full = FALSE) {
  out <- df %>%
    filter(party %in% c("Democrat", "Republican")) %>%
    dplyr::summarise(
      `Toxic` = mean(TOXICITY, na.rm = TRUE),
      `Obscene` = mean(OBSCENE, na.rm = TRUE),
      `Identity Attack` = mean(IDENTITY_ATTACK, na.rm = TRUE),
      n = n(),
      sd_toxic = sd(TOXICITY, na.rm = TRUE),
      sd_obscene = sd(OBSCENE, na.rm = TRUE),
      sd_identity = sd(IDENTITY_ATTACK, na.rm = TRUE),
      se_toxic = sd_toxic / sqrt(n),
      se_obscene = sd_obscene / sqrt(n),
      se_identity = sd_identity / sqrt(n)
    )
  if (full == FALSE) {
    out %>%
      party_factor(., outvar = "type")
  } else {
    out %>%
      mutate(financial = TRUE) %>%
      mutate(
        type = factor(
          type,
          levels = rev(c(
            "WinRed", "Other Rep.\nPlatform", "ActBlue", "Other Dem.\nPlatform"
          ))
        )
      )
  }
}

# Wayback-specific Functions ===================================================
wayback_memento <- function(files) {
  files %>%
    map_dfr(
      ~ tryCatch(
        {
          read.table(.x, sep = ";", fill = TRUE, as.is = TRUE)
        },
        error = function(e) {
          NULL
        }
      )
    ) %>%
    select(-V4) %>%
    rename(link = V1, rel = V2, dt = V3) %>%
    mutate(
      ## referenced wayback::get_mementos
      link = stringi::stri_replace_all_regex(trimws(link), "^<|>$", ""),
      rel = stringi::stri_replace_all_regex(trimws(rel), "^rel=|,", ""),
      dt = parse_date_time(
        stringi::stri_replace_all_regex(trimws(dt), "^datetime=|,| GMT", ""),
        orders = "adbyHMS"
      )
    ) %>%
    filter(rel == "first memento" | rel == "memento")
}

wayback_timemap <- function(df, i, var = "url") {
  paste0(
    "http://web.archive.org/web/timemap/link/",
    df[[var]][i]
  )
}

file_pattern <- function(df, i, var = "url", senate = FALSE) {
  if (!("state_cd" %in% names(df)) | senate == TRUE) {
    # Senate
    out <- paste0(
      df$state[i], "-SEN_",
      ## trimws(tolower(df$first_name[i])), "_",
      trimws(tolower(df$last_name[i]))
    )
  } else {
    # House
    out <- paste0(
      df$state_cd[i], "_",
      ## trimws(tolower(df$first_name[i])), "_",
      trimws(tolower(df$last_name[i]))
    )
  }
  if (var != "campaign_website") {
    ## out <- paste0(out, "_", trimws(tolower(df$url_end[i])))
    out <- out %>%
      gsub("?iframe=true", "", .)
  }
  out <- out %>%
    gsub("\\s+", "_", .) %>%
    gsub("_+", "_", .) %>%
    gsub("[^[:alnum:]_-]", "", .)

  return(out)
}

wayback_std <- function(df, var = "url") {
  df %>%
    ungroup() %>%
    mutate(
      !!as.name(var) :=
        trimws(tolower(gsub("http://", "https://", !!as.name(var)))),
      !!as.name(var) := gsub(
        paste0(
          "\\.$|\\./$|:80$|:80/$|:80/\\?$||:443$|:443/$|:443/\\?$|\\?$|",
          "#$|:80/home$|:443/home|:80/home/$|:443/home/|/%252f$|",
          "/home$|/home/$|:80|",
          "https://web.archive.org/screenshot/"
        ),
        "", !!as.name(var)
      ),
      !!as.name(var) := gsub("/{2,}", "/", !!as.name(var)),
      !!as.name(var) := gsub("https:/", "https://", !!as.name(var)),
      ## beckwith for congress
      !!as.name(var) := gsub("^ttps://", "https://www", !!as.name(var)),
      !!as.name(var) := gsub("^ttps:/p", "https://www.p", !!as.name(var)),
      !!as.name(var) := gsub("https://wwwwww", "https://www", !!as.name(var)),
      !!as.name(var) := gsub("/+$", "", !!as.name(var))
    ) %>%
    rowwise() %>%
    mutate(
      !!as.name(var) := ifelse(
        grepl("www", !!as.name(var)),
        !!as.name(var),
        gsub("://", "://www.", !!as.name(var))
      ),
      !!as.name(var) := ifelse(
        !grepl("https://", !!as.name(var)) & grepl("^www", !!as.name(var)),
        paste0("https://", !!as.name(var)),
        !!as.name(var)
      )
    ) %>%
    ungroup()
}

wayback_merge <- function(df, campaigns, var) {
  df %>%
    mutate(date = as_date(dt)) %>%
    rowwise() %>%
    # Initial dataframe will always have the following columns:
    # link, rel, dt
    mutate(!!as.name(var) := str_match(link, pattern = "/(http.*)$")[1, 2]) %>%
    wayback_std(., var = var) %>%
    wayback_std(., var = var) %>%
    group_by(!!as.name(var), date) %>%
    slice(1) %>%
    ungroup() %>%
    mutate(year = year(date)) %>%
    filter(!grepl("@", !!as.name(var))) %>%
    filter(
      ## https://web.archive.org/web/20141115202623/https://www.act.myngp.com/forms/-566609128118550528
      ## Not sure why recorded as 2014, but definitely *not* 2014
      ## Limit was not 2800 at the time
      grepl("566609128118550528", !!as.name(var)) | (year >= 2019)
    ) %>%
    left_join(
      .,
      campaigns %>%
        ## select(-year) %>%
        wayback_std(., var = var) %>%
        wayback_std(., var = var) %>%
        dedup() %>%
        select(!!as.name(var), everything())
    )
}

wayback_msg <- function(df, i, ...) {
  paste0(
    i, "-th row (", file_pattern(df, i, ...), ", ", df$dt[i], ") completed."
  )
}

wayback_stamp_html <- function(df, i, fp) {
  type <- ifelse(grepl("front", fp), "front", "donation")
  here(
    gsub("timestamp_donation", "html", fp),
    paste0(
      file_pattern(
        df, i,
        var = ifelse(type == "donation", "url", "campaign_website")
      ), "_",
      format(df$dt[i], "%Y%m%d%H%M%S"), "_", type, ".html"
    )
  )
}

wayback_timemap_exceptions <- function(campaigns, fp, var = "url") {
  setdiff(
    seq(nrow(campaigns)) %>%
      map(
        ~ here(
          fp,
          paste0(
            "wayback_timemap_",
            file_pattern(campaigns, .x, var = var), ".txt"
          )
        )
      ) %>%
      unlist(),
    list.files(here(fp), pattern = ".txt", full.names = TRUE)
  ) %>%
    map(
      ~ gsub(c(here(), fp) %>% paste(collapse = "|"), "", .x) %>%
        gsub("//", "", .)
    ) %>%
    unlist()
}

actblue_js <- function(url) {
  x <- read_html(url) %>%
    html_nodes(xpath = '//*[@type="text/javascript"]') %>%
    html_text()

  temp <- x %>%
    map(
      ~ {
        temp <- str_match_all(.x, "window.indigoListResponse = (\\{.*\\})")
        if (!is.null(temp)) {
          temp[[1]][, 2]
        }
      }
    ) %>%
    unlist() %>%
    fromJSON()

  # temp$entities
  # temp three types: vector, dataframe, value
  temp_df <- temp %>%
    map(is.data.frame) %>%
    unlist() %>%
    which() %>%
    names()

  temp_vc <- temp %>%
    map(~ length(.x) > 1 & !is.data.frame(.x)) %>%
    unlist() %>%
    which() %>%
    names()

  json_rest <- names(temp) %>%
    map(
      ~ {
        if (
          !is.null(temp[[.x]]) &
            !(
              .x %in% c(
                # "entities", "post_donation_upsells", "brandings",
                temp_df,
                # "relevant_surrogate_keys", "share_content", "radio_amounts",
                # "eligibility_values", "eligibility_values_es",
                # "list_disclaimer_policy", "acceptable_card_types",
                # "managing_entity", "fundraising_video", "custom_fields"
                temp_vc
              )
            )
        ) {
          tibble(!!as.name(.x) := temp[[.x]])
        } else {
          NULL
        }
      }
    ) %>%
    keep(~ !is.null(.x)) %>%
    keep(~ nrow(.x) > 0) %>%
    bind_cols()

  out <- tibble(url = url, js_rest = json_rest, date = Sys.Date())
  for (i in c(temp_df, temp_vc)) {
    out[[i]] <- list(temp[[i]])
  }
  assert_that(nrow(out) == 1)

  return(out)
}

winred_text_scrape <- function(x) {
  ## Metadata
  temp <- read_html(x) %>%
    html_nodes("meta")
  meta_data <- landing_text <- landing_logo <- landing_bgimg <-
    landing_footer <- NA

  meta_data <- temp %>% {
    tibble(
      name = html_attr(., "name"),
      property = html_attr(., "property"),
      content = html_attr(., "content")
    )
  }

  ## Landing page text
  landing_text <- read_html(x) %>%
    html_nodes(".landing-page-paragraph") %>%
    html_text()

  landing_text <- gsub("\\s+", " ", gsub("\n", " ", landing_text)) %>%
    trimws() %>%
    unique()

  if (length(landing_text) == 0) landing_text <- ""

  landing_logo <- read_html(x) %>%
    html_nodes(".landing-page-image") %>%
    html_nodes("img") %>%
    html_attr("src")
  if (length(landing_logo) == 0) landing_logo <- ""

  landing_bgimg <- read_html(x) %>%
    html_nodes(".landing-page-mobile-image") %>%
    html_nodes("source") %>%
    html_attr("srcset")
  if (length(landing_bgimg) == 0) landing_bgimg <- ""

  landing_footer <- read_html(x) %>%
    html_nodes(".landing-page-footer") %>%
    html_text()
  if (length(landing_footer) == 0) landing_footer <- ""

  out <- meta_data %>%
    mutate(name = coalesce(name, property)) %>%
    filter(!is.na(name)) %>%
    select(-property) %>%
    pivot_wider(values_from = content) %>%
    janitor::clean_names()

  if (nrow(out) == 1 & length(landing_text) > 1) {
    ## crenshaw; two landing paragraphs
    landing_text <-
      landing_text[which(nchar(landing_text) == max(nchar(landing_text)))]
  }

  out <- out %>%
    mutate(
      text = landing_text,
      footer = landing_footer,
      logo = landing_logo,
      bgimg = landing_bgimg,
      date = Sys.Date()
    )

  return(out)
}

anedot_text_scrape <- function(x) {
  ## Metadata
  temp <- read_html(x) %>%
    html_nodes("meta")
  meta_data <- landing_text <- landing_logo <- landing_bgimg <- NA

  meta_data <- temp %>% {
    tibble(
      name = html_attr(., "name"),
      property = html_attr(., "property"),
      content = html_attr(., "content")
    )
  }

  ## Landing page text
  landing_text <- read_html(x) %>%
    html_nodes("div") %>%
    html_nodes(
      xpath = paste0(
        '//*[@class="donations--form--',
        'campaign-description text-styles--campaign-description"]'
      )
    ) %>%
    html_text()
  if (length(landing_text) == 0) landing_text <- ""

  landing_logo <- read_html(x) %>%
    html_nodes("img") %>%
    html_nodes(xpath = '//*[@class="logo"]') %>%
    html_attr("src")
  if (length(landing_logo) == 0) landing_logo <- ""

  landing_bgimg <- read_html(x) %>%
    html_nodes("style") %>%
    html_text() %>%
    map(
      ~ {
        out <- str_match_all(.x, 'background-image: url\\(\\"(.*?)\"\\)')[[1]]
        if (length(out) > 0) {
          out[1, 2]
        }
      }
    ) %>%
    keep(~ !is.null(.x)) %>%
    unlist() %>%
    unique()

  if (length(landing_bgimg) == 0) landing_bgimg <- ""

  # Skip footer
  # landing_footer <- read_html(x) %>%
  #   html_nodes(".landing-page-footer") %>%
  #   html_text()

  out <- meta_data %>%
    mutate(name = coalesce(name, property)) %>%
    filter(!is.na(name)) %>%
    select(-property) %>%
    pivot_wider(values_from = content) %>%
    janitor::clean_names()

  out <- out %>%
    mutate(
      text = landing_text,
      # footer = landing_footer,
      logo = landing_logo,
      bgimg = landing_bgimg,
      date = Sys.Date()
    )

  return(out)
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

removing_tokens <- c(
  "rt", "amp", "u8", "<p>", "<", ">", "div", "img", "alt", "br",
  "text-align", "li", "=", "b", "nbsp", "style",
  "em", "p", "strong", "center", "u", "href", "rel"
)

## https://facebookresearch.github.io/Radlibrary/index.html
## See also
## https://disinfo.quaidorsay.fr/en/facebook-ads-library-assessment

# Converts Radlibrary response object to tibble, creating list columns for list fields
parse_response <- function(response, list_cols) {
  stopifnot(
    "'response' must be an 'adlib_data_response' object" =
      inherits(response, "adlib_data_response")
  )
  response_data <- response$data
  purrr::map(response_data, list2tibble, list_cols = list_cols) %>%
    dplyr::bind_rows()
}

# Convert demographic data fields to tibbles
# and extract upper-lower bound fields into separate columns.
# Fills missing fields with explicit NULL.
list2tibble <- function(l, list_cols, numeric_cols = "percentage") {
  missing <- setdiff(list_cols, names(l))
  present <- setdiff(list_cols, missing)

  # Different types of list field
  multivalued <- c("demographic_distribution", "region_distribution")
  upper_lower <- c("potential_reach", "spend", "impressions")
  bounds <- c("lower_bound", "upper_bound")
  l[missing] <- list(NULL)

  # Convert demographic data fields to tibbles,
  # fill in missing fields for upper-lower bound fields,
  # and don't modify unnamed list fields
  l[present] <- purrr::map2(l[present], present, function(col, col_name) {
    if (col_name %in% upper_lower) {
      col[bounds] <- lapply(col[bounds], as.numeric)
      col[setdiff(names(col), bounds)] <- NA_real_
      out <- col
    } else if (col_name %in% multivalued) {
      out <- dplyr::bind_rows(col)
      # Recode numeric values if present
      if (length(numerics <- intersect(numeric_cols, colnames(out))) > 0) {
        out <- list(dplyr::mutate(out, across(!!!numerics, as.numeric)))
      }
      # Just treat unknown kind as list column
    } else {
      out <- list(col)
    }
    out
  })
  # Least offensive way I could find to flatten these fields,
  # which are lists with elements named "upper_bound" and "lower_bound"
  bound_cols <- intersect(upper_lower, present)
  if (length(bound_cols) > 0) {
    flattened <- unlist(l[bound_cols], recursive = FALSE)
    names(flattened) <- gsub("\\.", "_", names(flattened))
    l[bound_cols] <- NULL
    c(l, flattened)
  } else {
    l
  }
}

# Partition a directory containing directories for k image classes into test, train, and validation directories, each with a specified proportion (rounded) of images, stored in k subdirectories
partition_data_dir <- function(data_dir = here::here("data", "classifier"),
                               proportions = c(train = 0.7, test = 0.15, valid = 0.15),
                               out_dir = file.path(data_dir, "trump_image"),
                               exclude_dirs = here::here("data/classifier", c("extra_image", "trump_image"))) {
  stopifnot(
    "Data directory must exist" = dir.exists(data_dir),
    "Data directory mus contain at least one directory marked for processing" = length(to_process <- list.dirs(data_dir, recursive = FALSE) %>%
      setdiff(exclude_dirs)) > 0,
    "Split proportions must sum to 1" = sum(proportions) == 1,
    "Splits must be named 'train', 'test', and 'valid'" = identical(
      sort(names(proportions)),
      c("test", "train", "valid")
    ),
    "Values must be valid proportions" = all(proportions >= 0 & proportions <= 1),
    "Directories must not already exist" = !any(dir.exists(dirs <- file.path(data_dir, names(proportions))))
  )
  splits <- lapply(to_process, partition_class_files, proportions = proportions) %>%
    unlist(recursive = FALSE) %>%
    split(names(.))

  if (!dir.exists(out_dir)) {
    dir.create(out_dir)
  }

  for (part in names(proportions)) {
    dir.create(file.path(out_dir, part))
    for (class_obs in splits[[part]]) {
      stopifnot("Each directory must contain only one class" = length(cur_class <- unique(names(class_obs))) == 1)
      # For each split, create containing directory, then add subdirectories for each class, the format expected by Torch
      new_dir <- file.path(out_dir, part, cur_class)
      dir.create(new_dir)
      file.copy(class_obs, file.path(new_dir, basename(class_obs)))
    }
  }
}

partition_class_files <- function(class_dir, proportions) {
  files <- list.files(class_dir, full.names = TRUE)
  stopifnot("Must have at least 5 observations to split" = (n_obs <- length(files)) >= 5)
  splits <- mapply(rep, names(proportions), each = pmax(floor(n_obs * proportions), 1)) %>%
    unlist()
  splits <- c(splits, rep("train", times = n_obs - length(splits)))[sample(n_obs, n_obs, replace = FALSE)]
  stopifnot(length(unique(splits)) == 3)
  names(files) <- rep(basename(class_dir), length(files))
  split(files, splits)
}
