# Setup ========================================================================
source(here::here("R", "utilities.R"))

# Copy the token directly from the Facebook Ad Library (will be under the
# 'Graph API Explorer' tool). Paste it into the console when prompted. Token
# will expire within an hour(?) of generation, so be mindful of that.
token <- readline()

# Data Wrangling ===============================================================
senate <- read_csv(
  here("data", "raw", "fb", "fb-senate.csv"), col_types = cols(.default = "c")
) %>%
  select(candidate, state, id = fb_ad_library_id) %>%
  ## 75 ---> 72
  filter(!is.na(id))

## Those who were not up for reelection
senate <- bind_rows(
  senate,
  read_csv(
    here("data", "raw", "fb", "fb_senate_not_running_2020.csv"),
    col_types = cols(.default = "c")
  ) %>%
    select(candidate, state = state_po, id = fb_ad_library_id) %>%
    filter(!is.na(id))
) %>%
  dedup()

## Radlibrary will only accept 10 page IDs at a time; run by each ID
assert_that(!any(duplicated(senate$candidate)))
assert_that(!any(grepl("E", senate$id)))
assert_that(!any(duplicated(senate$id)))

senate_list <- senate %>%
  group_by(id) %>%
  group_split() %>%
  `names<-`({.} %>% map(~ .x$candidate[1]) %>% unlist())

# Run FB Graph API =============================================================
fname1 <- here("data", "raw", "fb", "fb-senate-raw-ad-data-2020.Rda")
fname2 <- here("data", "raw", "fb", "fb-senate-raw-demo-data-2020.Rda")
fname3 <- here("data", "raw", "fb", "fb-senate-raw-region-data-2020.Rda")
if (file.exists(fname1)) {
  load(fname1)
  load(fname2)
  load(fname3)
} else {
  ad_senate <- vector("list", length(senate_list))
  names(ad_senate) <- names(senate_list)
  region_senate <- demo_senate <- ad_senate
}

for (i in seq(length(senate_list))) {
  idx <- senate_list[[i]]$id
  cand <- senate_list[[i]]$candidate
  message(paste0("Queued for ", cand, ", ", senate_list[[i]]$state, "."))
  Sys.sleep(3)
  
  if (!grepl("e", tolower(idx))) {
    if (is.null(ad_senate[[cand]])) {
      ## Three chunks of preset groups of data
      ad_senate[[cand]] <-
        fb_short(id = idx, token = token, fields = "ad_data")
      assert_that(!is.null(ad_senate[[cand]]))
      Sys.sleep(3)
      
      demo_senate[[cand]] <- 
        fb_short(id = idx, token = token, fields = "demographic_data")
      assert_that(!is.null(demo_senate[[cand]]))
      Sys.sleep(3)
      
      region_senate[[cand]] <- 
        fb_short(id = idx, token = token, fields = "region_data")
      assert_that(!is.null(region_senate[[cand]]))
      Sys.sleep(3)
      
      print(head(ad_senate[[cand]]$tbl))
    }
  }
  save(ad_senate, file = fname1)
  save(demo_senate, file = fname2)
  save(region_senate, file = fname3)
  message(paste0("Finished for ", cand, ", ", senate_list[[i]]$state, "."))
  message(paste0("Row ", i, " out of ", length(senate_list), "."))
}

# Accidentally left out candidates? ============================================
## Results are odd, considering ads like 
## https://www.facebook.com/ads/library/?id=296408818376585
## Barrasso in 2020 cycle
ad_senate %>% map("tbl") %>% map_lgl(is.null) %>% which()

# Which candidates go over 5,000? ==============================================
vec <- ad_senate %>%
  map("tbl") %>%
  map(nrow) %>% 
  ## map_lgl gives type compliance errors:
  ## must be a single logical, not a logical vector of length 0
  map(~ .x >= 5000) %>%
  unlist() %>%
  which() %>%
  names()

idx_retry <- senate_list %>% 
  map("candidate") %>%
  map_lgl(~ .x %in% vec) %>%
  which()

# [1] "STEVE BULLOCK"        "AMY MCGRATH"          "CAL CUNNINGHAM"      
# [4] "Bernie Sanders"       "MARK KELLY"           "LINDSEY GRAHAM"      
# [7] "SARA I. GIDEON"       "CORY BOOKER"          "Elizabeth Warren"    
# [10] "JEFF MERKLEY"         "Michael Bennet"       "Kirsten Gillibrand"  
# [13] "JOHN W. HICKENLOOPER"

date_breaks <- c(
  seq(as.Date("2019-01-01"), as.Date("2020-12-31"), by = "1 week"),
  as.Date("2020-12-31")
)
## Bullock/McGrath/Cunningham/Kelly/Merkley/Hickenlooper okay with monthly cuts
## Graham okay with 2-week cuts
## Bennet/Gillibrand/Booker/Gideon/Warren okay with 1-week cuts
## Sanders too much data (apparently 1 day is too much for Sanders)
## Weeks 8, 9, 13, 23, 26, 29, 44, 60, 61, 62

# incomplete_weeks <- date_breaks[c(8, 9, 13, 23, 26, 29, 44, 60, 61, 62)]
# incomplete_days <- incomplete_weeks %>%
#   map(~ seq(.x, .x + 7, by = "1 day")) %>%
#   unlist() %>%
#   unique()
# 
# for (x in seq(length(incomplete_days))) {
#   ad_senate[[cand]][[x + 200]] <- fb_short(
#     id = idx, token = token, fields = "ad_data", 
#     min_date = as.Date(incomplete_days[x], origin = "1970-01-01"),
#     max_date = as.Date(incomplete_days[x], origin = "1970-01-01")
#   )
#   Sys.sleep(3)
#   
#   demo_senate[[cand]][[x + 200]] <- fb_short(
#     id = idx, token = token, fields = "demographic_data", 
#     min_date = as.Date(incomplete_days[x], origin = "1970-01-01"),
#     max_date = as.Date(incomplete_days[x], origin = "1970-01-01")
#   )
#   Sys.sleep(3)
#   
#   region_senate[[cand]][[x + 200]] <- fb_short(
#     id = idx, token = token, fields = "region_data", 
#     min_date = as.Date(incomplete_days[x], origin = "1970-01-01"),
#     max_date = as.Date(incomplete_days[x], origin = "1970-01-01")
#   )
#   Sys.sleep(3)
#   
#   if (!is.null(nrow(ad_senate[[cand]][[x]]$tbl))) {
#     ## This checks that the smaller interval is safe and not 
#     ## overflowing with ads
#     assert_that(nrow(ad_senate[[cand]][[x]]$tbl) < 5000)
#     message(as.Date(incomplete_days[x], origin = "1970-01-01"), " done.")
#     message(paste0("Number of rows was ", nrow(ad_senate[[cand]][[x]]$tbl)))
#   }
#   Sys.sleep(5)
# }

for (i in idx_retry) {
  idx <- senate_list[[i]]$id
  cand <- senate_list[[i]]$candidate
  region_senate[[cand]] <- demo_senate[[cand]] <- ad_senate[[cand]] <- list() 
  ## Run by two months intervals then combine the rows
  for (x in (seq(length(date_breaks) - 1))) {
    ad_senate[[cand]][[x]] <- fb_short(
      id = idx, token = token, fields = "ad_data", 
      min_date = date_breaks[x],
      max_date = date_breaks[x + 1]
    )
    Sys.sleep(3)

    demo_senate[[cand]][[x]] <- fb_short(
      id = idx, token = token, fields = "demographic_data", 
      min_date = date_breaks[x],
      max_date = date_breaks[x + 1]
    )
    Sys.sleep(3)
    
    region_senate[[cand]][[x]] <- fb_short(
      id = idx, token = token, fields = "region_data", 
      min_date = date_breaks[x],
      max_date = date_breaks[x + 1]
    )
    Sys.sleep(3)
    
    if (!is.null(nrow(ad_senate[[cand]][[x]]$tbl))) {
      ## This checks that the smaller interval is safe and not 
      ## overflowing with ads
      assert_that(nrow(ad_senate[[cand]][[x]]$tbl) < 5000)
      message(paste0("1 week interval from ", date_breaks[x], " done."))
      message(paste0("Number of rows was ", nrow(ad_senate[[cand]][[x]]$tbl)))
    }
    Sys.sleep(5)
  }

  ## Bind rows
  ad_senate[[cand]]$tbl <- ad_senate[[cand]] %>% map("tbl") %>% bind_rows()
  demo_senate[[cand]]$tbl <- demo_senate[[cand]] %>% map("tbl") %>% bind_rows()
  region_senate[[cand]]$tbl <- 
    region_senate[[cand]] %>% map("tbl") %>% bind_rows()
  
  assert_that(!is.null(ad_senate[[cand]]))
  ## assert_that(nrow(ad_senate[[cand]]$tbl) > 5000)
  save(ad_senate, file = fname1)
  save(demo_senate, file = fname2)
  save(region_senate, file = fname3)
  message(paste0("Finished for ", cand, ", ", senate_list[[i]]$state, "."))
}

# Final check ==================================================================
## They must all equal
ad_senate %>%
  map("tbl") %>%
  map(nrow) %>%
  unlist() %>%
  length()
demo_senate %>%
  map("tbl") %>%
  map(nrow) %>%
  unlist() %>%
  length()
region_senate %>%
  map("tbl") %>%
  map(nrow) %>%
  unlist() %>%
  length()

# Prep to merge with ID ========================================================
## ad's adlib_id is demo's and region's id
## API token accidentally included in URL for demo and region; delete
ad <- ad_senate %>%
  map_dfr("tbl", .id = "candidate") %>%
  dedup() %>%
  rename(id = adlib_id)

demo <- demo_senate %>%
  map_dfr("tbl", .id = "candidate") %>%
  dedup() %>%
  select(-ad_snapshot_url)

region <- region_senate %>%
  map_dfr("tbl", .id = "candidate") %>%
  dedup() %>%
  select(-ad_snapshot_url)

# Sanity checks ================================================================
assert_that(!any(duplicated(ad$id)))
assert_that(!any(duplicated(demo$id)))
assert_that(!any(duplicated(region$id)))

nrow(ad) ## 260934
nrow(demo) ## 260934
nrow(region) ## 260934

assert_that(length(setdiff(ad$id, demo$id)) == 0)
assert_that(length(setdiff(demo$id, ad$id)) == 0)
assert_that(length(setdiff(region$id, ad$id)) == 0)
assert_that(length(setdiff(ad$id, region$id)) == 0)

# Unnest and pivot demo and region =============================================
## Note that there are actually no-target ads by demo/region
## e.g., 672752909991155 by Amy Kennedy
demo <- demo %>%
  unnest(cols = c(demographic_distribution)) %>%
  unite("demo", c("gender", "age"), sep = ", ") %>%
  pivot_wider(
    id_cols = c(candidate, id), names_from = "demo", values_from = "percentage"
  ) %>%
  clean_names()

region <- region %>%
  unnest(cols = c(region_distribution)) %>%
  rename(stname = region) %>%
  mutate(stname = tolower(stname)) %>%
  left_join(
    ., Kmisc::fips %>%
      select(stname, stabb) %>%
      mutate(
        stname = ifelse(
          stname == "district of columbia", 
          "washington, district of columbia", stname
        )
      )
  ) %>%
  ## Those without state names: either "unknown" or outside U.S. mainland
  ## e.g., British Columbia, Ontario, England, ... 
  select(-stname) %>%
  ## So it requires grouping and combining percentages due to NAs
  group_by(candidate, id, stabb) %>% 
  summarise(percentage = sum(percentage, na.rm = TRUE)) %>%
  pivot_wider(
    id_cols = c(candidate, id), names_from = "stabb", values_from = "percentage"
  ) %>%
  clean_names()

# Final merge and save =========================================================
fb_senate <- left_join(ad, left_join(demo, region))
save(fb_senate, file = here("data", "tidy", "fb_senate_merged.Rda"))
