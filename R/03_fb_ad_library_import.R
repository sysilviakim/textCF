source(here::here("R", "utilities.R"))
token <- readline() ## FB Access Token; do not store

congress_fb <- c(senate = "senate", house = "house") %>%
  imap(
    ~ here("data", "raw", "fb", paste0("fb-", .x, ".csv")) %>%
      read_csv(col_types = cols(.default = col_character())) %>%
      select(-state_po, -state_fips, -state_cen, -state_ic) %>%
      select(-contains("party_simplified")) %>%
      select(
        contains("state"), candidate, party_detailed, fb_ad_library_id,
        everything()
      )
  )

congress_fb <- c(senate = "senate", house = "house") %>%
  map(
    ~ congress_fb[[.x]] %>%
      select(
        - {
          congress_fb[[.x]] %>%
            map_dbl(~ length(table(.x, useNA = "ifany"))) %>%
            {which(. == 1)} %>%
            names()
        }
      )
  )

