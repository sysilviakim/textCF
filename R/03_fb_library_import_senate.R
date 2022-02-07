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

# [1] "AMY MCGRATH"          "CAL CUNNINGHAM"       "MARK KELLY"          
# [4] "LINDSEY GRAHAM"       "SARA I. GIDEON"       "CORY BOOKER"         
# [7] "JEFF MERKLEY"         "JOHN W. HICKENLOOPER"

date_breaks <- c(
  seq(as.Date("2019-01-01"), as.Date("2020-12-31"), by = "1 month"),
  as.Date("2020-12-31")
)
for (i in idx_retry) {
  idx <- senate_list[[i]]$id
  cand <- senate_list[[i]]$candidate
  ad_senate[[cand]] <- list() 
  ## Run by two months intervals then combine the rows
  for (x in seq(length(date_breaks) - 1)) {
    ad_senate[[cand]][[x]] <- fb_short(
      id = idx, token = token, fields = "ad_data", 
      min_date = date_breaks[x],
      max_date = date_breaks[x + 1]
    )
    demo_senate[[cand]][[x]] <- fb_short(
      id = idx, token = token, fields = "demographic_data", 
      min_date = date_breaks[x],
      max_date = date_breaks[x + 1]
    )
    region_senate[[cand]][[x]] <- fb_short(
      id = idx, token = token, fields = "region_data", 
      min_date = date_breaks[x],
      max_date = date_breaks[x + 1]
    )
    if (!is.null(nrow(ad_senate[[cand]][[x]]$tbl))) {
      assert_that(nrow(ad_senate[[cand]][[x]]$tbl) < 5000)
      message(paste0("1mo interval starting from ", date_breaks[x], " done."))
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
  assert_that(nrow(ad_senate[[cand]]$tbl) > 5000)
  save(ad_house, file = fname1)
  save(demo_house, file = fname2)
  save(region_house, file = fname3)
  message(paste0("Finished for ", cand, ", ", senate_list[[i]]$state, "."))
}

# Final check ==================================================================
ad_senate %>% map("tbl") %>% map(nrow) %>% unlist() %>% length()
