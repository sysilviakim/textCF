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
  group_split() %>%
  `names<-`({.} %>% map(~ .x$candidate[1]) %>% unlist())

# Run FB Graph API =============================================================
fname <- here("data", "raw", "fb", "fb-house-raw-ads-2020.Rda")
if (!file.exists(fname)) {
  load(fname)
} else {
  fb_house <- vector("list", length(house_list))
  names(fb_house) <- names(house_list)
}

for (i in seq(length(house_list))) {
  idx <- house_list[[i]]$id
  cand <- house_list[[i]]$candidate
  message(paste0("Queued for ", cand, ", ", house_list[[i]]$state, "."))
  Sys.sleep(3)
  
  if (!grepl("e", tolower(idx))) {
    if (is.null(fb_house[[cand]])) {
      fb_house[[cand]] <- fb_short(id = idx, token = token)
      assert_that(!is.null(fb_house[[cand]]))
      print(head(fb_house[[cand]]$tbl))
      Sys.sleep(5)
    }
  }
  save(fb_house, file = fname)
  message(paste0("Finished for ", cand, ", ", house_list[[i]]$state, "."))
  message(paste0("Row ", i, " out of ", length(house_list), "."))
}

# Removing Duplicates, and Saving Data =========================================
fb.house.distinct <- distinct(fb.house)
save(fb.house.distinct, file = "data/raw/fb/fb-house-2020-ads.rda")

# For Loop Experimentation =====================================================
loopdat <- housedat.01
loopdatvec <- as.vector(loopdat$id)
for (i in loopdatvec) {
  test_query_i <- adlib_build_query(
    ad_reached_countries = "US",
    ad_active_status = "ALL",
    search_page_ids = as.vector(i),
    ad_delivery_date_max = "2020-11-03",
    ad_delivery_date_min = "2020-01-01",
    limit = 5000,
    fields = "ad_data"
  )
  test_response_i <- adlib_get(params = test_query_i, token = token)
  test_tibble_i <- as_tibble(test_response_i, type = "ad")
}
## I can't figure out how to craft a for loop that'll work for Radlibrary.
## It only pulls two ads from one candidate.

loopdf <- as.data.frame(loopdatvec)
for (i in loopdf) {
  test_query_i <- adlib_build_query(
    ad_reached_countries = "US",
    ad_active_status = "ALL",
    search_page_ids = as.vector(i),
    ad_delivery_date_max = "2020-11-03",
    ad_delivery_date_min = "2020-01-01",
    limit = 5000,
    fields = "ad_data"
  )
  test_response_i <- adlib_get(params = test_query_i, token = token)
  test_tibble_i <- as_tibble(test_response_i, type = "ad")
}
# The issue here is, this is putting all of the i's into one query. This is
# basically just a different version of the basic version of the code in the
# chunks above. It wouldn't work if there were more than 10 items.
# By way of example:
testwith12 <- head(housedat, 12)
testwith12vec <- as.vector(testwith12$id)
testwith12df <- as.data.frame(testwith12vec)
for (i in testwith12df) {
  test_query_i <- adlib_build_query(
    ad_reached_countries = "US",
    ad_active_status = "ALL",
    search_page_ids = as.vector(i),
    ad_delivery_date_max = "2020-11-03",
    ad_delivery_date_min = "2020-01-01",
    limit = 5000,
    fields = "ad_data"
  )
  test_response_i <- adlib_get(params = test_query_i, token = token)
  test_tibble_i <- as_tibble(test_response_i, type = "ad")
}
# It doesn't work. "Error in adlib_build_query(ad_reached_countries = "US",
# ad_active_status = "ALL",  : Can only search 10 page IDs at a time."


# The loop needs to run multiple queries and return multiple data frames for
# it to be useful. Otherwise, it's counterintuitive.
