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
## devtools::install_github("kbenoit/quanteda.dictionaries")
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
## devtools::install_github("facebookresearch/Radlibrary")
library(Radlibrary)
library(knitr)
library(kableExtra)
library(stargazer)
library(RColorBrewer)
library(gender)

# install.packages("devtools")
## devtools::install_github("hrbrmstr/wayback")
library(wayback)

# Non-CRAN packages ============================================================
## devtools::install_github("sysilviakim/Kmisc")
library(Kmisc)

# Dictionaries =================================================================
## Moral Foundations Dictionary 2.0
## https://osf.io/ezn37/
moral <- dictionary(file = here("data/raw/dictionaries/mfd2.0.dic"))

## Trolling Dictionary
## https://osf.io/bm8uy/
# troll <- read_csv(
#   file = here(
#     "data", "raw", "dictionaries",
#     "troll_and_divide/troll_and_divide_Glove_Expansion_Raters_phase_2.csv"
#   )
# ) %>%
#   pull(Word) %>%
#   list(troll = .) %>%
#   dictionary()

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

## misnamed
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
      scale_x_continuous(limits = c(0, 0.151))
  } else {
    pdf_default(p) +
      theme(legend.position = "none") +
      scale_x_continuous(limits = c(0, 0.151))
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
      `Toxic` = mean(toxicity, na.rm = TRUE),
      `Obscene` = mean(obscene, na.rm = TRUE),
      n = n(),
      sd_toxic = sd(toxicity, na.rm = TRUE),
      sd_obscene = sd(obscene, na.rm = TRUE),
      se_toxic = sd_toxic / sqrt(n),
      se_obscene = sd_obscene / sqrt(n)
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

percentage_calc <- function(f, c, p = TRUE) {
  if (p == TRUE) {
    x1 <- summ_df %>%
      filter(
        grepl("Rep", party) & grepl(f, financial) & chamber == c
      ) %>%
      .$Toxic
    x2 <- summ_df %>%
      filter(
        grepl("Dem", party) & grepl(f, financial) & chamber == c
      ) %>%
      .$Toxic
  } else if (p == "Democrat" | p == "Republican") {
    x1 <- summ_df %>%
      filter(grepl(p, party) & grepl("Donor", financial) & chamber == c) %>%
      .$Toxic
    x2 <- summ_df %>%
      filter(grepl(p, party) & grepl("Voter", financial) & chamber == c) %>%
      .$Toxic
  }
  return((x1 - x2) / x2 * 100)
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

unique_sanity_check <- function(x, var) {
  x %>%
    select(candidate, !!as.name(var)) %>%
    distinct() %>%
    group_by(candidate) %>%
    filter(n() > 1) %>%
    nrow()
}
