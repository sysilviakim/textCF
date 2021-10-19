source(here::here("R", "utilities.R"))

# Raw data import (output from solicitRdesc) ===================================
## Set of general election candidates that should be in the data
load(here("data", "raw", "cong-filtered.Rda"))

# Read candidate-level text data scraped in Feb 2021 ===========================
cong_text <- c(house = "house", senate = "senate") %>%
  map(
    function(x) c("actblue", "anedot", "winred") %>%
      set_names(., .) %>%
      imap(
        ~ loadRData(
          here(
            "data/raw/2022", x, 
            ifelse (
              .y == "actblue", "actblue_js_scraped.Rda",
              paste0(.y, "_text_scraped.Rda")
            )
          )
        )
      )
  )

# names(cong_text$house$actblue)
# names(cong_text$house$anedot)
# names(cong_text$house$winred)

## Extremely different data scraped from three domains
## Filter only what I need

# Check for missing instances: Senate ==========================================
temp <- bind_rows(
  actblue_select_text(cong_text$senate$actblue),
  winred_select_text(cong_text$senate$winred)
) %>%
  mutate(
    last_name = tolower(trimws(last_name)),
    first_name = tolower(trimws(first_name))
  )

senate <- cong_filtered$senate %>%
  left_join(., temp)

## 38.7% missing. Ouch!
sum(is.na(senate$text)) / nrow(senate) * 100 

# Check for missing instances: House ===========================================
temp <- bind_rows(
  actblue_select_text(cong_text$house$actblue),
  winred_select_text(cong_text$house$winred)
) %>%
  mutate(
    last_name = tolower(trimws(last_name)),
    first_name = tolower(trimws(first_name))
  )

house <- cong_filtered$house %>%
  left_join(., temp)

## 79.0% missing. Yikes!
sum(is.na(house$text)) / nrow(house) * 100 




