source(here::here("R", "utilities.R"))
token <- readline() ## FB Access Token; do not store

# Import data for FB IDs =======================================================
## MIT elections data (general candidates) + FB ad manual additions (RA)
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

# Delete no-variation data =====================================================
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

# Senate query =================================================================
## Loop, so that it won't be lost
query_senate_2020 <- vector("list", length = nrow(congress_fb$senate))
names(query_senate_2020) <- congress_fb$senate$fb_ad_library_id
for (idx in congress_fb$senate$fb_ad_library_id) {
  ## Build list
  query_senate_2020[[idx]] <- list(ad = NA, demo = NA, region = NA)
  
  ## Build first query: ad
  query <- fb_query_short(idx, fields = "ad_data")
  response <- adlib_get(params = query, token = token)
  query_senate_2020[[idx]]$ad <- as_tibble(response, type = "ad")
  
  ## Build first query: demo
  query <- fb_query_short(idx, fields = "demographic_data")
  response <- adlib_get(params = query, token = token)
  query_senate_2020[[idx]]$demo <- as_tibble(response, type = "ad")
  
  ## Build first query: ad
  query <- fb_query_short(idx, fields = "region_data")
  response <- adlib_get(params = query, token = token)
  query_senate_2020[[idx]]$region <- as_tibble(response, type = "ad")
}

# Check why adlib_get_paginated =/= adlib_get
result_paginated <- adlib_get_paginated(query, max_gets = 1e8, token = token)
assert_that(!result_paginated$has_next)

df_paginated <- as_tibble(result_paginated, type = "ad")
dim(df_paginated)
