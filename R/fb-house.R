# Basics =======================================================================

## Download and load any necessary packages
#install.packages("devtools")
#devtools::install_github("facebookresearch/Radlibrary")
library(Radlibrary)

# Setup ========================================================================

# Copy the token directly from the Facebook Ad Library (will be under the
# 'Graph API Explorer' tool). Paste it into the console when prompted. Token 
# will expire within an hour(?) of generation, so be mindful of that.
# For me, it is no more than an hour 
token <- readline()

# Data Wrangling ===============================================================

fb_house <- read.csv("data/raw/fb/fb-house.csv")
housedat <- as.data.frame(cbind(fb_house$candidate, fb_house$state,
                                fb_house$fb_ad_library_id))
colnames(housedat) <- c("candidate", "state", "id")
housedat$id <- as.numeric(housedat$id)
housedat <- housedat[!is.na(housedat$id),] # Takes us from 868 to 736
# Now, the rub is that the Radlibrary will only accept 10 page IDs at a time.
# So, we should split this dataframe up into 74 dataframes with rows of 10.
houseframes <- split(housedat,rep(1:74,each=10))
# It gave a warning that the data length is not a multiple of the split 
# variable, but it's okay - the 74th dataframe has a length of 6 rows.
#housedat.01 <- houseframes[["1"]]

# Group 1 ======================================================================

hr.id.1a <- as.vector(houseframes[["1"]]$id)

hr.query.1a <- adlib_build_query(ad_reached_countries = 'US', 
                                  ad_active_status = 'ALL',
                                  search_page_ids = hr.id.1a,
                                  ad_delivery_date_max = '2020-11-03',
                                  ad_delivery_date_min = '2020-09-25',
                                  limit = 5000,
                                  fields = "ad_data")
# This is by delivery date, which makes it even more difficult to parse out 
# what to do...
## I thought this approach would work better, but it seems I was wrong

hr.response.1a <- adlib_get(params = hr.query.1a, token = token)
hr.tibble.1a <- as_tibble(hr.response.1a, type = "ad")

# For loop test ================================================================

for (i in 2:10) {
  hr.id.i <- as.vector(houseframes[[as.character(i)]]$id)
  hr.query.i <- adlib_build_query(ad_reached_countries = 'US', 
                                 ad_active_status = 'ALL',
                                 search_page_ids = hr.id.i,
                                 ad_delivery_date_max = '2020-11-03',
                                 ad_delivery_date_min = '2020-09-25',
                                 limit = 5000,
                                 fields = "ad_data")
  hr.response.i <- adlib_get(params = hr.query.i, token = token)
  hr.tibble.i <- as_tibble(hr.response.i, type = "ad")
  
}

# To test and see if this worked, let's see how many unique ids are in this
# resulting hr.tibble.i
length(unique(hr.tibble.i$page_id))
# This did not work - it did not give us the range for 2-10, and does not seem
