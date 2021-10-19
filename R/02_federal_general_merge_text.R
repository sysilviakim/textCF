source(here::here("R", "utilities.R"))

# Raw data import (output from solicitRdesc) ===================================
## Set of general election candidates that should be in the data
load(here("data", "raw", "cong-filtered.Rda"))

# Match to federal candidate data scraped in Feb 2021 ==========================
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
  
