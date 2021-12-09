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
if (file.exists(fname)) {
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

# Which candidates go over 5,000? ==============================================
vec <- fb_senate %>%
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
  seq(as.Date("2019-01-01"), as.Date("2020-12-31"), by = "2 months"),
  as.Date("2020-12-31")
)
for (i in idx_retry) {
  idx <- senate_list[[i]]$id
  cand <- senate_list[[i]]$candidate
  fb_senate[[cand]] <- list() 
  ## Run by two months intervals then combine the rows
  for (x in seq(length(date_breaks) - 1)) {
    fb_senate[[cand]][[x]] <- fb_short(
      id = idx, token = token, limit = 1e7,
      min_date = date_breaks[x],
      max_date = date_breaks[x + 1]
    )
    if (!is.null(nrow(fb_senate[[cand]][[x]]$tbl))) {
      assert_that(nrow(fb_senate[[cand]][[x]]$tbl) < 5000)
    }
    message(paste0("2 month interval starting from ", date_breaks[x], " done."))
    Sys.sleep(5)
  }
  fb_senate[[cand]]$tbl <- fb_senate[[cand]] %>% map("tbl") %>% bind_rows()
  assert_that(!is.null(fb_senate[[cand]]))
  save(fb_senate, file = fname)
  message(paste0("Finished for ", cand, ", ", senate_list[[i]]$state, "."))
}

