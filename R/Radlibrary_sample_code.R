# Facebook Ad Library sample code - Ossoff and Perdue ==========================

# Basics =======================================================================

## Download and load any necessary packages
#install.packages("devtools")
#devtools::install_github("facebookresearch/Radlibrary")
# library(Radlibrary)

# Setup ========================================================================

# Copy the token directly from the Facebook Ad Library (will be under the
# 'Graph API Explorer' tool). Paste it into the console when prompted. Token 
# will expire within an hour(?) of generation, so be mindful of that.
# For me, it is no more than an hour 
token <- readline()

# Since we are looking for Jon Ossoff and David Perdue's pages, we need to
# obtain their Facebook Page IDs. These are numeric chains that typically form
# the final chunk of the url. For official pages, at least for these, the last
# chunk was the candidate's name, or a variant thereof, so it is necessary to
# use View Source on the candidate's home page and search for their page_id
# there.

ossoff_id <- 1336293769761923
perdue_id <- 159597424207730
id_vector <- as.vector(c(ossoff_id, perdue_id))

# Building a Query =============================================================

# The following query will obtain the 1000 most recent ads from Jon Ossoff and 
# David Perdue.

query <- adlib_build_query(ad_reached_countries = 'US', 
                           ad_active_status = 'ALL',
                           search_page_ids = id_vector,
                           fields = "ad_data")

response <- adlib_get(params = query, token = token)
results.tibble <- as_tibble(response, type = "ad")

#save(results.tibble, file = "ad_library_ossoff_perdue.rda")

# More tailored approach =======================================================

# The code above does yield data, but it has flaws.
# For one thing, it doesn't give us a sense of the regions, or of other 
# relevant data. For another, it does not properly specify a date range.
# This is something that can be dealt with, although the Radlibrary might not
# have the best tools to get to the bottom of all of them.

states <- c('Alabama', 'Arizona', 'Arkansas', 'Alaska', 'Connecticut', 
            'Delaware', 'Florida', 'Georgia')

query2 <- adlib_build_query(ad_reached_countries = 'US', 
                           ad_active_status = 'ALL',
                           search_page_ids = id_vector,
                           ad_delivery_date_max = '2021-01-03',
                           ad_delivery_date_min = '2020-01-01',
                           delivery_by_region = states,
                           fields = "ad_data")
response2 <- adlib_get(params = query2, token = token)
results.tibble <- as_tibble(response2, type = "ad")
#save(results.tibble, file = "ad_library_ossoff_perdue_2.rda")
# Not actually saving this, but this is how we might do so.
# This data provides all ads that aired in the eight specified states between
# January 1st, 2020, and January 3rd, 2021. 

# This is better, but it still leaves much to be desired.
# When we browse the Facebook Ad Library, we can see the breakdown of where an
# ad was aired when we view the details of this ad on a mass scale.
# However, I (Brian) have not yet figured out how to pull this data using the
# Radlibrary package on a large scale - pulling this data by hand is definitely
# not feasible.

# Before we go on, let's pull the IDs for all the candidates.

# House Candidate IDs ==========================================================
fb_house <- read.csv("data/raw/fb/fb-house.csv")
#View(fb_house)
housedat <- as.data.frame(cbind(fb_house$candidate, fb_house$state,
                                  fb_house$fb_ad_library_id))
colnames(housedat) <- c("candidate", "state", "id")
#class(housedat$id)
# This variable must be numeric
housedat$id <- as.numeric(housedat$id)
housedat <- housedat[!is.na(housedat$id),] # Takes us from 868 to 736
# Now, the rub is that the Radlibrary will only accept 10 page IDs at a time.
# So, we should split this dataframe up into 74 dataframes with rows of 10.
houseframes <- split(housedat,rep(1:74,each=10))
# It gave a warning that the data length is not a multiple of the split 
# variable, but it's okay - the 74th dataframe has a length of 6 rows.
#housedat.01 <- houseframes[["1"]]
# Now, a for loop would be superfluous here. We have all we need already.
# In order to get the IDs in, say, the 29th dataframe, we would call:
houseframes[["29"]]$id
# Plugging this in should work - let's test it out
# Let's figure out what states...
houseframes[["29"]]$state # Kansas and Kentucky... maybe we should sort these
## by state?

states2 <- c('Kansas', 'Kentucky')
house29ids <- as.vector(houseframes[["29"]]$id)

library(Radlibrary)
query3 <- adlib_build_query(ad_reached_countries = 'US', 
                            ad_active_status = 'ALL',
                            search_page_ids = house29ids,
                            ad_delivery_date_max = '2020-11-03',
                            ad_delivery_date_min = '2020-01-01',
                            delivery_by_region = states2,
                            fields = "ad_data")
response3 <- adlib_get(params = query3, token = token)
results.tibble <- as_tibble(response3, type = "ad")
# Not sure what the issue is...I can't load Facebook on any browser right now...
# Or on my phone for that matter. My token expired, and I can't generate a new
# one.
# "Request failed [503]"...I think this is a server issue.

# Senate Candidate IDs =========================================================
fb_senate <- read.csv("data/raw/fb/fb-senate.csv")
senatedat <- as.data.frame(cbind(fb_senate$candidate, fb_senate$state,
                                fb_senate$fb_ad_library_id))
colnames(senatedat) <- c("candidate", "state", "id")
senatedat$id <- as.numeric(senatedat$id)
senatedat <- senatedat[!is.na(senatedat$id),] # Takes us from 75 to 72
senateframes <- split(senatedat,rep(1:8,each=10))
