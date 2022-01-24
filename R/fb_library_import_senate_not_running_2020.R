# Setup ========================================================================
source(here::here("R", "utilities.R"))

# Copy the token directly from the Facebook Ad Library (will be under the
# 'Graph API Explorer' tool). Paste it into the console when prompted. Token
# will expire within an hour(?) of generation, so be mindful of that.
token <- readline()

# Data Wrangling ===============================================================
senate_other <- read_csv(
  here("data", "raw", "fb", "fb_senate_not_running_2020.csv"), 
  col_types = cols(.default = "c")
) %>%
  select(candidate, state_po, id = fb_ad_library_id) %>%
  ## 66 ---> 62
  filter(!is.na(id))

## Radlibrary will only accept 10 page IDs at a time; run by each ID
assert_that(!any(duplicated(senate_other$candidate)))
assert_that(!any(grepl("E", senate_other$id)))
assert_that(!any(duplicated(senate_other$id)))
# All True!

senate_other_list <- senate_other %>%
  group_by(id) %>%
  group_split() %>%
  `names<-`({.} %>% map(~ .x$candidate[1]) %>% unlist())

# Run FB Graph API =============================================================
fname <- here("data", "raw", "fb", "fb-senate-other-raw-ads-2020.Rda")
if (file.exists(fname)) {
  load(fname)
} else {
  fb_senate_other <- vector("list", length(senate_other_list))
  names(fb_senate_other) <- names(senate_other_list)
}

for (i in seq(length(senate_other_list))) {
  idx <- senate_other_list[[i]]$id
  cand <- senate_other_list[[i]]$candidate
  message(paste0("Queued for ", cand, ", ", senate_other_list[[i]]$state, "."))
  Sys.sleep(3)
  
  if (!grepl("e", tolower(idx))) {
    if (is.null(fb_senate_other[[cand]])) {
      fb_senate_other[[cand]] <- fb_short(id = idx, token = token)
      assert_that(!is.null(fb_senate_other[[cand]]))
      print(head(fb_senate_other[[cand]]$tbl))
      Sys.sleep(5)
    }
  }
  save(fb_senate_other, file = fname)
  message(paste0("Finished for ", cand, ", ", senate_other_list[[i]]$state, "."))
  message(paste0("Row ", i, " out of ", length(senate_other_list), "."))
}

# Which candidates go over 5,000? ==============================================
vec <- fb_senate_other %>%
  map("tbl") %>%
  map(nrow) %>% 
  ## map_lgl gives type compliance errors:
  ## must be a single logical, not a logical vector of length 0
  map(~ .x >= 5000) %>%
  unlist() %>%
  which() %>%
  names()

idx_retry <- senate_other_list %>% 
  map("candidate") %>%
  map_lgl(~ .x %in% vec) %>%
  which()

# Bernie Sanders (6), Elizabeth Warren (31), Michael Bennet (39), 
# Kirsten Gillibrand (50)

date_breaks <- c(
  seq(as.Date("2019-01-01"), as.Date("2020-12-31"), by = "1 month"),
  as.Date("2020-12-31")
)
for (i in idx_retry) {
  idx <- senate_other_list[[i]]$id
  cand <- senate_other_list[[i]]$candidate
  fb_senate_other[[cand]] <- list() 
  ## Run by two months intervals then combine the rows
  for (x in seq(length(date_breaks) - 1)) {
    fb_senate_other[[cand]][[x]] <- fb_short(
      id = idx, token = token, 
      min_date = date_breaks[x],
      max_date = date_breaks[x + 1]
    )
    if (!is.null(nrow(fb_senate_other[[cand]][[x]]$tbl))) {
      assert_that(nrow(fb_senate_other[[cand]][[x]]$tbl) < 5000)
      message(paste0("1mo interval starting from ", date_breaks[x], " done."))
      message(paste0("Number of rows was ", nrow(fb_senate_other[[cand]][[x]]$tbl)))
    }
    Sys.sleep(5)
  }
  fb_senate_other[[cand]]$tbl <- fb_senate_other[[cand]] %>% map("tbl") %>% bind_rows()
  assert_that(!is.null(fb_senate_other[[cand]]))
  assert_that(nrow(fb_senate_other[[cand]]$tbl) > 5000)
  save(fb_senate_other, file = fname)
  message(paste0("Finished for ", cand, ", ", senate_other_list[[i]]$state, "."))
}

