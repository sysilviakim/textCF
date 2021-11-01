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
# risk...we don't want duplicated ads in our final sample.

# Further splitting ============================================================

#senateframes2 <- split(senatedat,rep(1:15,each=5))
# We splilt the data into frames with a maximum of 5 members per frame this time

#senateidsA <- as.vector(senateframes2[["1"]]$id)

#sen.query1 <- adlib_build_query(ad_reached_countries = 'US', 
#                                ad_active_status = 'ALL',
#                                search_page_ids = senateidsA,
#                                ad_delivery_date_max = '2020-11-03',
#                                ad_delivery_date_min = '2020-01-01',
#                                limit = 10000,
#                                fields = "ad_data")
# This query will pull all of the ads for the first 5 senators (assuming they
# have less than ten thousand ads between them) between January 1st, 2020 and
# Election Day 2020.

#sen.response1 <- adlib_get(params = sen.query1, token = token)
#sen.tibble1 <- as_tibble(sen.response1, type = "ad")

# Even here, we hit the 5,000 cap in July - better than September, but not great

#senateframes3 <- split(senatedat,rep(1:24,each=3))

#senateidsB <- as.vector(senateframes3[["1"]]$id)

#sen.query1 <- adlib_build_query(ad_reached_countries = 'US', 
#                                ad_active_status = 'ALL',
#                                search_page_ids = senateidsB,
#                                ad_delivery_date_max = '2020-11-03',
#                                ad_delivery_date_min = '2020-01-01',
#                                limit = 10000,
#                                fields = "ad_data")
# This query will pull all of the ads for the first 3 senators (assuming they
# have less than ten thousand ads between them) between January 1st, 2020 and
# Election Day 2020.

#sen.response1 <- adlib_get(params = sen.query1, token = token)
#sen.tibble1 <- as_tibble(sen.response1, type = "ad")

# Hopefully this will keep working for the remaining 23 frames

# Frame 2:
## >5,000 - would have to break it up into multiple groups...
#senateidsB2 <- as.vector(senateframes3[["2"]]$id)
#sen.query2 <- adlib_build_query(ad_reached_countries = 'US', 
#                                ad_active_status = 'ALL',
#                                search_page_ids = senateidsB2,
#                                ad_delivery_date_max = '2020-11-03',
#                                ad_delivery_date_min = '2020-01-01',
#                                limit = 10000,
#                                fields = "ad_data")
#sen.response2 <- adlib_get(params = sen.query2, token = token)
#sen.tibble2 <- as_tibble(sen.response2, type = "ad")

# Ultimately, what makes the most sense, I think, will be to use the blocks of
# ten candidates apiece, run tests, find the last date that ads were pulled on,
# and use that date as the min date for the first round and the max for the
# next round

# Ultimate Approach ============================================================

# Repeated from earlier - I intend to cut out the extraneous stuff, and replace
# it with comments about why I'm doing this approach, at the end

#senateframes <- split(senatedat,rep(1:8,each=10))
# We splilt the data into frames with a maximum of 10 members per frame
# because we can only query the ads for ten pages at a time.


#senate1ids <- as.vector(senateframes[["1"]]$id)

#sen.query1 <- adlib_build_query(ad_reached_countries = 'US', 
#                                ad_active_status = 'ALL',
#                                search_page_ids = senate1ids,
#                                ad_delivery_date_max = '2020-11-03',
#                                ad_delivery_date_min = '2020-01-01',
#                                limit = 10000,
#                                fields = "ad_data")
# This query will pull all of the ads for the first ten senators (assuming they
# have less than ten thousand ads between them) between January 1st, 2020 and
# Election Day 2020.

#sen.response1 <- adlib_get(params = sen.query1, token = token)
#sen.tibble1 <- as_tibble(sen.response1, type = "ad")

## Group 1 =====================================================================

senate.1.ids <- as.vector(senateframes[["1"]]$id)

sen.query.1a <- adlib_build_query(ad_reached_countries = 'US', 
                                ad_active_status = 'ALL',
                                search_page_ids = senate.1.ids,
                                ad_delivery_date_max = '2020-11-03',
                                ad_delivery_date_min = '2020-09-25',
                                limit = 5000,
                                fields = "ad_data")
# This is by delivery date, which makes it even more difficult to parse out 
# what to do...
## I thought this approach would work better, but it seems I was wrong

sen.response.1a <- adlib_get(params = sen.query.1a, token = token)
sen.tibble.1a <- as_tibble(sen.response.1a, type = "ad")
