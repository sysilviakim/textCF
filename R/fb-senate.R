# Basics =======================================================================

## Download and load any necessary packages
#install.packages("devtools")
#devtools::install_github("facebookresearch/Radlibrary")
#library(Radlibrary)

# Setup ========================================================================

# Copy the token directly from the Facebook Ad Library (will be under the
# 'Graph API Explorer' tool). Paste it into the console when prompted. Token 
# will expire within an hour(?) of generation, so be mindful of that.
# For me, it is no more than an hour 
token <- readline()

# Data Wrangling ===============================================================

fb_senate <- read.csv("data/raw/fb/fb-senate.csv")
senatedat <- as.data.frame(cbind(fb_senate$candidate, fb_senate$state,
                                 fb_senate$fb_ad_library_id))
colnames(senatedat) <- c("candidate", "state", "id")
senatedat$id <- as.numeric(senatedat$id)
senatedat <- senatedat[!is.na(senatedat$id),] # Takes us from 75 to 72
senateframes <- split(senatedat,rep(1:8,each=10))
# We splilt the data into frames with a maximum of 10 members per frame
# because we can only query the ads for ten pages at a time.


senate1ids <- as.vector(senateframes[["1"]]$id)

sen.query1 <- adlib_build_query(ad_reached_countries = 'US', 
                            ad_active_status = 'ALL',
                            search_page_ids = senate1ids,
                            ad_delivery_date_max = '2020-11-03',
                            ad_delivery_date_min = '2020-01-01',
                            limit = 10000,
                            fields = "ad_data")
# This query will pull all of the ads for the first ten senators (assuming they
# have less than ten thousand ads between them) between January 1st, 2020 and
# Election Day 2020.

sen.response1 <- adlib_get(params = sen.query1, token = token)
sen.tibble1 <- as_tibble(sen.response1, type = "ad")

# An issue has arisen. The query has only pulled the last 5,000 observations,
# not the 10,000 requested. Perhaps this is the outright cap for queries?

# Perhaps splitting further, or else having multiple queries for each group
# arranging by date? I think splitting further would run the lowest amount of
# risk...
