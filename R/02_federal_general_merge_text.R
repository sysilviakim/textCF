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

# Wrangling data ===============================================================
## Extremely different data scraped from three domains
## Filter only what I need

# Check for missing instances: Senate ==========================================
senate <- cong_filtered$senate %>%
  left_join(., actblue_select_text(cong_text$senate$actblue))

# Check for missing instances: House ===========================================



