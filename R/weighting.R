source(here::here("R", "utilities.R"))

# Load data ====================================================================
fb_house <- read_rds("data/tidy/fb_perspective_House.RDS")
fb_senate <- read_rds("data/tidy/fb_perspective_Senate.RDS")
load(here("data", "tidy", "fb_matched.Rda"))

## This is only to set up the code, because the Perspective data is currently
## (as of today, 4/26/22) out of date. I am working through the code and making
## comments as I go for now, but such things will be edited as appropriate or 
## removed when the code is run with updated perspective data

# Merging weighting variables into fb_house ====================================

# Key variables we need:
## Date: ad_creation_time
## Impressions: impressions_lower, impressions_upper
## Money: spend_lower, spend_upper
# Need to merge... will use fb_ad_library_id, ad_creative_body -- different
## capitalization of candidate variables will cause headaches

# Sanity checks, before we get started...
assert_that(!any(is.na(fb_house$ad_creative_body)))
assert_that(!any(is.na(fb_matched$house$ad_creative_body)))
## Not True...since this is about the ad texts, we'll remove ones without the
## ad creative body
fb_matched$house <- fb_matched$house[!is.na(fb_matched$house$ad_creative_body),]
assert_that(!any(is.na(fb_matched$house$ad_creative_body))) # True

house_weights <- merge(
  x = fb_house,
  y = fb_matched$house[, c("fb_ad_library_id", "ad_creative_body", 
                           "ad_creation_time",
                           "impressions_lower", "impressions_upper",
                           "spend_lower", "spend_upper"
                           )],
  by = c("fb_ad_library_id", "ad_creative_body"), all.x = TRUE
)
# Sanity checks
assert_that(!any(is.na(house_weights$ad_creative_body))) # True
assert_that(!any(is.na(house_weights$TOXICITY))) # True
#assert_that(!any(is.na(house_weights$ad_creation_time)))
# NOT True...because I forgot to remove Michael San Nicolas from the toxicity
# data
house_weights <- house_weights[
  !(house_weights$candidate == "MICHAEL F. Q. SAN NICOLAS"),
]
assert_that(!any(is.na(house_weights$ad_creation_time)))# True
# Are there any duplicated rows? 
#assert_that(!any(duplicated(house_weights)))
# Will include code to remove them -- however, for the actual run, will double-
# check about this step
house_weights <- house_weights[!duplicated(house_weights), ]
# Number of rows goes significantly down with this...before, though, it was
# greater than the number of rows in fb_matched$house, which didn't make sense

# Merging weighting variables into fb_senate ===================================

# Sanity checks, before we get started...
assert_that(!any(is.na(fb_senate$ad_creative_body)))
assert_that(!any(is.na(fb_matched$senate$ad_creative_body)))
## Not True...since this is about the ad texts, we'll remove ones without the
## ad creative body
fb_matched$senate <- fb_matched$senate[!is.na(
  fb_matched$senate$ad_creative_body),]
assert_that(!any(is.na(fb_matched$senate$ad_creative_body)))
# True now
nrow(fb_matched$senate)
senate_weights <- merge(
  x = fb_senate,
  y = fb_matched$senate[, c("fb_ad_library_id", "ad_creative_body", 
                           "ad_creation_time",
                           "impressions_lower", "impressions_upper",
                           "spend_lower", "spend_upper"
  )],
  by = c("fb_ad_library_id", "ad_creative_body"), all.x = TRUE
)
# Sanity checks
assert_that(!any(is.na(senate_weights$ad_creative_body))) # True
assert_that(!any(is.na(senate_weights$TOXICITY))) # True
#assert_that(!any(is.na(senate_weights$ad_creation_time)))
# Not True...because Alex Padilla is still in the Perspective data
senate_weights <- senate_weights[
  !(senate_weights$candidate == "Alex Padilla"),
]
assert_that(!any(is.na(senate_weights$ad_creation_time)))

# Are there any duplicated rows? 
#assert_that(!any(duplicated(senate_weights)))
# Same deal as with house
senate_weights <- senate_weights[!duplicated(senate_weights), ]
assert_that(!any(duplicated(senate_weights)))

