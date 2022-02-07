# Setup ========================================================================
source(here::here("R", "utilities.R"))

# Copy the token directly from the Facebook Ad Library (will be under the
# 'Graph API Explorer' tool). Paste it into the console when prompted. Token
# will expire within an hour(?) of generation, so be mindful of that.
token <- readline()

# Data Importing/wrangling =====================================================
house <- read_csv(
  here("data", "raw", "fb", "fb-house.csv"), col_types = cols(.default = "c")
) %>%
  select(candidate, state, id = fb_ad_library_id) %>%
  ## 868 ---> 736
  filter(!is.na(id)) %>%
  mutate(
    ## RAdlibrary IDs corrupted somewhere: names and corrections ---------------
    ## Yvette Herrell, NM, 1750700000000000    ---> 1750699881894920
    ## Ted Budd, NC, 1585290000000000          ---> 1585291608451100
    ## Tony Gonzales, TX, 2146200000000000     ---> 2146201085498440
    ## Jaimy Z. Blanco, TX, 1773150000000000   ---> 1773152856029750
    ## Genevieve Collins, TX, 2338980000000000 ---> 2338979479650120
    ## Nicholas Betts, VA, 1911030000000000    ---> 1911027705653860
    ## Donald Beyer, VA, 1411980000000000      ---> 1411983362382190
    id = case_when(
      candidate == "YVETTE HERRELL" & grepl("E", id) ~ "1750699881894920",
      candidate == "TED BUDD" & grepl("E", id) ~ "1585291608451100",
      candidate == "TONY GONZALES" & grepl("E", id) ~ "2146201085498440",
      candidate == "JAIMY Z. BLANCO" & grepl("E", id) ~ "1773152856029750",
      candidate == "GENEVIEVE COLLINS" & grepl("E", id) ~ "2338979479650120",
      candidate == "NICHOLAS A. BETTS" & grepl("E", id) ~ "1911027705653860",
      candidate == "DONALD S. BEYER,  JR." & grepl("E", id) ~
        "1411983362382190",
      TRUE ~ id
    )
  )

## Radlibrary will only accept 10 page IDs at a time; run by each ID
assert_that(!any(duplicated(house$candidate)))
assert_that(!any(grepl("E", house$id)))
assert_that(!any(is.na(house$id)))

## assert_that(!any(duplicated(house$id))) ---> not TRUE when it should be!
## C. ANTONIO DELGADO    CALIFORNIA    359153387814647
## ANTONIO DELGADO       NEW YORK      359153387814647 ---> probably mistake

## Group by ID but name with candidate
house_list <- house %>%
  group_by(id) %>%
  slice(1) %>% 
  group_split() %>%
  `names<-`({.} %>% map(~ .x$candidate[1]) %>% unlist())

# Run FB Graph API =============================================================
fname1 <- here("data", "raw", "fb", "fb-house-raw-ad-data-2020.Rda")
fname2 <- here("data", "raw", "fb", "fb-house-raw-demo-data-2020.Rda")
fname3 <- here("data", "raw", "fb", "fb-house-raw-region-data-2020.Rda")
if (file.exists(fname1)) {
  load(fname1)
  load(fname2)
  load(fname3)
} else {
  ad_house <- vector("list", length(house_list))
  names(ad_house) <- names(house_list)
  region_house <- demo_house <- ad_house
}

## 129 ADAM B. SCHIFF, CALIFORNIA?
## Please reduce the amount of data you're asking for, then retry your request
for (i in seq(length(house_list))) {
  idx <- house_list[[i]]$id
  cand <- house_list[[i]]$candidate
  message(paste0("Queued for ", cand, ", ", house_list[[i]]$state, "."))
  Sys.sleep(3)
  
  if (!grepl("e", tolower(idx))) {
    if (is.null(ad_house[[cand]])) {
      ## Three chunks of preset groups of data
      ad_house[[cand]] <-
        fb_short(id = idx, token = token, fields = "ad_data")
      assert_that(!is.null(ad_house[[cand]]))
      Sys.sleep(3)
      
      demo_house[[cand]] <- 
        fb_short(id = idx, token = token, fields = "demographic_data")
      assert_that(!is.null(demo_house[[cand]]))
      Sys.sleep(3)
      
      region_house[[cand]] <- 
        fb_short(id = idx, token = token, fields = "region_data")
      assert_that(!is.null(region_house[[cand]]))
      Sys.sleep(3)
      
      print(head(ad_house[[cand]]$tbl))
    }
  }
  save(ad_house, file = fname1)
  save(demo_house, file = fname2)
  save(region_house, file = fname3)
  message(paste0("Finished for ", cand, ", ", house_list[[i]]$state, "."))
  message(paste0("Row ", i, " out of ", length(house_list), "."))
}

# ad_house$`C. ANTONIO DELGADO`$tbl <- 
#   ad_house$`C. ANTONIO DELGADO` %>% map("tbl") %>% bind_rows() %>% dedup()

# Accidentally left out candidates? ============================================
ad_house %>% map("tbl") %>% map_lgl(is.null) %>% which()

# Which candidates go over 5,000? ==============================================
vec <- ad_house %>%
  map("tbl") %>%
  map(nrow) %>% 
  ## map_lgl gives type compliance errors:
  ## must be a single logical, not a logical vector of length 0
  map(~ .x >= 5000) %>%
  unlist() %>%
  which() %>%
  names()

idx_retry <- house_list %>% 
  map("candidate") %>%
  map_lgl(~ .x %in% vec) %>%
  which()

# [1] "WENDY R. DAVIS"           "ALEXANDRIA OCASIO-CORTEZ" "PETER A. DEFAZIO"
## Ended up re-doing work with a 1-week interval for AOC;
## Still ended up with errors for 87, 93, 94, 95 at region_house
## "Please reduce the amount of data you're asking for, then retry your request"
## Sometimes it works when re-run after some time (e.g., 93, 95)
## sometimes... it doesn't.
## It's unfortunately bit random

## Manually re-run region_house for "2020-08-25" -- "2020-08-26" ---> 87
##                                  "2020-08-27" -- "2020-08-28" ---> 106
##                                  "2020-08-29" -- "2020-08-30" ---> 107
##                                  "2020-08-31" -- "2020-09-01" ---> 108
##                                  "2020-10-13" -- "2020-10-15" ---> 94
##                                  "2020-10-16" -- "2020-10-18" ---> 109
##                                  "2020-10-19" -- "2020-10-20" ---> 110

## Best course of action is to use two-day or one-day interval for AOC :)

date_breaks <- c(
  ## Due to AOC, 1mo not sufficiently small interval
  seq(as.Date("2019-01-01"), as.Date("2020-12-31"), by = "1 week"),
  as.Date("2020-12-31")
)
for (i in idx_retry) {
  idx <- house_list[[i]]$id
  cand <- house_list[[i]]$candidate
  region_house[[cand]] <- demo_house[[cand]] <- ad_house[[cand]] <- list() 
  ## Run by two months intervals then combine the rows
  for (x in seq(length(date_breaks) - 1)) {
    ad_house[[cand]][[x]] <- fb_short(
      id = idx, token = token, fields = "ad_data", 
      min_date = date_breaks[x],
      max_date = date_breaks[x + 1]
    )
    demo_house[[cand]][[x]] <- fb_short(
      id = idx, token = token, fields = "demographic_data", 
      min_date = date_breaks[x],
      max_date = date_breaks[x + 1]
    )
    region_house[[cand]][[x]] <- fb_short(
      id = idx, token = token, fields = "region_data", 
      min_date = date_breaks[x],
      max_date = date_breaks[x + 1]
    )
    if (!is.null(nrow(ad_house[[cand]][[x]]$tbl))) {
      assert_that(nrow(ad_house[[cand]][[x]]$tbl) < 5000)
      message(paste0("1-week interval from ", date_breaks[x], " done."))
      message(paste0("Number of rows was ", nrow(ad_house[[cand]][[x]]$tbl)))
    }
    Sys.sleep(5)
  }

  ## Bind rows
  ad_house[[cand]]$tbl <- ad_house[[cand]] %>% map("tbl") %>% bind_rows()
  demo_house[[cand]]$tbl <- demo_house[[cand]] %>% map("tbl") %>% bind_rows()
  region_house[[cand]]$tbl <- 
    region_house[[cand]] %>% map("tbl") %>% bind_rows()
  
  assert_that(!is.null(ad_house[[cand]]))
  assert_that(nrow(ad_house[[cand]]$tbl) > 5000)
  save(ad_house, file = fname1)
  save(demo_house, file = fname2)
  save(region_house, file = fname3)
  message(paste0("Finished for ", cand, ", ", house_list[[i]]$state, "."))
}

# Final check ==================================================================
## They must all equal and be 654
ad_house %>% map("tbl") %>% map(nrow) %>% unlist() %>% length()
demo_house %>% map("tbl") %>% map(nrow) %>% unlist() %>% length()
region_house %>% map("tbl") %>% map(nrow) %>% unlist() %>% length()
