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

## Radlibrary will only accept 10 page IDs at a time; run by each ID
assert_that(!any(duplicated(senate$candidate)))
assert_that(!any(grepl("E", senate$id)))
assert_that(!any(duplicated(senate$id)))

senate_list <- senate %>%
  group_by(id) %>%
  group_split() %>%
  `names<-`({.} %>% map(~ .x$candidate[1]) %>% unlist())

# Run FB Graph API =============================================================
fname <- here("data", "raw", "fb", "fb-senate-raw-ads-2020.Rda")
if (file.exists(fname)) {
  load(fname)
} else {
  fb_senate <- vector("list", length(senate_list))
  names(fb_senate) <- names(senate_list)
}

for (i in seq(length(senate_list))) {
  idx <- senate_list[[i]]$id
  cand <- senate_list[[i]]$candidate
  message(paste0("Queued for ", cand, ", ", senate_list[[i]]$state, "."))
  Sys.sleep(3)
  
  if (!grepl("e", tolower(idx))) {
    if (is.null(fb_senate[[cand]])) {
      fb_senate[[cand]] <- fb_short(id = idx, token = token)
      assert_that(!is.null(fb_senate[[cand]]))
      print(head(fb_senate[[cand]]$tbl))
      Sys.sleep(5)
    }
  }
  save(fb_senate, file = fname)
  message(paste0("Finished for ", cand, ", ", senate_list[[i]]$state, "."))
  message(paste0("Row ", i, " out of ", length(senate_list), "."))
}
