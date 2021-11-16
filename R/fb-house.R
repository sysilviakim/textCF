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

# Missing Candidates ===========================================================

# I learned the hard way that Radlibrary does not accept page IDs that are
# greater than 15 characters. Some candidates are accordingly excluded (as of 
# this point). Those candidates. and their states and page IDs, are as follows:

## Yvette Herrell, NM, 1750700000000000
## Ted Budd, NC, 1585290000000000
## Tony Gonzales, TX, 2146200000000000
## Jaimy Z. Blanco, TX, 1773150000000000
## Genevieve Collins, TX, 2338980000000000
## Nicholas Betts, VA, 1911030000000000
## Donald Beyer, VA, 1411980000000000


# As integer, perhaps? The float error
#vec %>% map_lgl(is.integer) %>% all()


# Group 1 ======================================================================

hr.id.1 <- as.vector(houseframes[["1"]]$id)
hr.query.1 <- adlib_build_query(ad_reached_countries = 'US', 
                                  ad_active_status = 'ALL',
                                  search_page_ids = hr.id.1,
                                  ad_delivery_date_max = '2020-11-03',
                                  ad_delivery_date_min = '2020-01-01',
                                  limit = 5000,
                                  fields = "ad_data")
hr.response.1 <- adlib_get(params = hr.query.1, token = token)
hr.tibble.1 <- as_tibble(hr.response.1, type = "ad")

# Group 2 ======================================================================

hr.id.2 <- as.vector(houseframes[["2"]]$id)
hr.query.2 <- adlib_build_query(ad_reached_countries = 'US', 
                                ad_active_status = 'ALL',
                                search_page_ids = hr.id.2,
                                ad_delivery_date_max = '2020-11-03',
                                ad_delivery_date_min = '2020-01-01',
                                limit = 5000,
                                fields = "ad_data")
hr.response.2 <- adlib_get(params = hr.query.2, token = token)
hr.tibble.2 <- as_tibble(hr.response.2, type = "ad")

fb.house <- rbind(hr.tibble.1, hr.tibble.2)

# Group 3 ======================================================================

hr.id.3 <- as.vector(houseframes[["3"]]$id)
hr.query.3 <- adlib_build_query(ad_reached_countries = 'US', 
                                ad_active_status = 'ALL',
                                search_page_ids = hr.id.3,
                                ad_delivery_date_max = '2020-11-03',
                                ad_delivery_date_min = '2020-01-01',
                                limit = 5000,
                                fields = "ad_data")
hr.response.3 <- adlib_get(params = hr.query.3, token = token)
hr.tibble.3 <- as_tibble(hr.response.3, type = "ad")

fb.house <- rbind(fb.house, hr.tibble.3)

# Group 4 ======================================================================

hr.id.4 <- as.vector(houseframes[["4"]]$id)
hr.query.4 <- adlib_build_query(ad_reached_countries = 'US', 
                                ad_active_status = 'ALL',
                                search_page_ids = hr.id.4,
                                ad_delivery_date_max = '2020-11-03',
                                ad_delivery_date_min = '2020-01-01',
                                limit = 5000,
                                fields = "ad_data")
hr.response.4 <- adlib_get(params = hr.query.4, token = token)
hr.tibble.4 <- as_tibble(hr.response.4, type = "ad")

fb.house <- rbind(fb.house, hr.tibble.4)

# Group 5 ======================================================================

hr.id.5 <- as.vector(houseframes[["5"]]$id)
hr.query.5 <- adlib_build_query(ad_reached_countries = 'US', 
                                ad_active_status = 'ALL',
                                search_page_ids = hr.id.5,
                                ad_delivery_date_max = '2020-11-03',
                                ad_delivery_date_min = '2020-01-01',
                                limit = 5000,
                                fields = "ad_data")
hr.response.5 <- adlib_get(params = hr.query.5, token = token)
hr.tibble.5 <- as_tibble(hr.response.5, type = "ad")

fb.house <- rbind(fb.house, hr.tibble.5)

# Group 6 ======================================================================

hr.id.6 <- as.vector(houseframes[["6"]]$id)
hr.query.6 <- adlib_build_query(ad_reached_countries = 'US', 
                                ad_active_status = 'ALL',
                                search_page_ids = hr.id.6,
                                ad_delivery_date_max = '2020-11-03',
                                ad_delivery_date_min = '2020-01-01',
                                limit = 5000,
                                fields = "ad_data")
hr.response.6 <- adlib_get(params = hr.query.6, token = token)
hr.tibble.6 <- as_tibble(hr.response.6, type = "ad")

fb.house <- rbind(fb.house, hr.tibble.6)

# Group 7 ======================================================================

hr.id.7 <- as.vector(houseframes[["7"]]$id)
hr.query.7 <- adlib_build_query(ad_reached_countries = 'US', 
                                ad_active_status = 'ALL',
                                search_page_ids = hr.id.7,
                                ad_delivery_date_max = '2020-11-03',
                                ad_delivery_date_min = '2020-01-01',
                                limit = 5000,
                                fields = "ad_data")
hr.response.7 <- adlib_get(params = hr.query.7, token = token)
hr.tibble.7 <- as_tibble(hr.response.7, type = "ad")

fb.house <- rbind(fb.house, hr.tibble.7)

# Group 8 ======================================================================

hr.id.8 <- as.vector(houseframes[["8"]]$id)
hr.query.8 <- adlib_build_query(ad_reached_countries = 'US', 
                                ad_active_status = 'ALL',
                                search_page_ids = hr.id.8,
                                ad_delivery_date_max = '2020-11-03',
                                ad_delivery_date_min = '2020-01-01',
                                limit = 5000,
                                fields = "ad_data")
hr.response.8 <- adlib_get(params = hr.query.8, token = token)
hr.tibble.8 <- as_tibble(hr.response.8, type = "ad")

fb.house <- rbind(fb.house, hr.tibble.8)

# Group 9 ======================================================================

hr.id.9 <- as.vector(houseframes[["9"]]$id)
hr.query.9 <- adlib_build_query(ad_reached_countries = 'US', 
                                ad_active_status = 'ALL',
                                search_page_ids = hr.id.9,
                                ad_delivery_date_max = '2020-11-03',
                                ad_delivery_date_min = '2020-01-01',
                                limit = 5000,
                                fields = "ad_data")
hr.response.9 <- adlib_get(params = hr.query.9, token = token)
hr.tibble.9 <- as_tibble(hr.response.9, type = "ad")

fb.house <- rbind(fb.house, hr.tibble.9)

# Group 10 =====================================================================

hr.id.10 <- as.vector(houseframes[["10"]]$id)
hr.query.10 <- adlib_build_query(ad_reached_countries = 'US', 
                                ad_active_status = 'ALL',
                                search_page_ids = hr.id.10,
                                ad_delivery_date_max = '2020-11-03',
                                ad_delivery_date_min = '2020-01-01',
                                limit = 5000,
                                fields = "ad_data")
hr.response.10 <- adlib_get(params = hr.query.10, token = token)
hr.tibble.10 <- as_tibble(hr.response.10, type = "ad")

fb.house <- rbind(fb.house, hr.tibble.10)

# Group 11 =====================================================================

hr.id.11 <- as.vector(houseframes[["11"]]$id)
hr.query.11 <- adlib_build_query(ad_reached_countries = 'US', 
                                 ad_active_status = 'ALL',
                                 search_page_ids = hr.id.11,
                                 ad_delivery_date_max = '2020-11-03',
                                 ad_delivery_date_min = '2020-01-01',
                                 limit = 5000,
                                 fields = "ad_data")
hr.response.11 <- adlib_get(params = hr.query.11, token = token)
hr.tibble.11 <- as_tibble(hr.response.11, type = "ad")

fb.house <- rbind(fb.house, hr.tibble.11)

# Group 12 =====================================================================

hr.id.12 <- as.vector(houseframes[["12"]]$id)
hr.query.12 <- adlib_build_query(ad_reached_countries = 'US', 
                                 ad_active_status = 'ALL',
                                 search_page_ids = hr.id.12,
                                 ad_delivery_date_max = '2020-11-03',
                                 ad_delivery_date_min = '2020-01-01',
                                 limit = 5000,
                                 fields = "ad_data")
hr.response.12 <- adlib_get(params = hr.query.12, token = token)
hr.tibble.12 <- as_tibble(hr.response.12, type = "ad")

fb.house <- rbind(fb.house, hr.tibble.12)

# Group 13 =====================================================================

hr.id.13 <- as.vector(houseframes[["13"]]$id)
hr.query.13 <- adlib_build_query(ad_reached_countries = 'US', 
                                 ad_active_status = 'ALL',
                                 search_page_ids = hr.id.13,
                                 ad_delivery_date_max = '2020-11-03',
                                 ad_delivery_date_min = '2020-01-01',
                                 limit = 5000,
                                 fields = "ad_data")
hr.response.13 <- adlib_get(params = hr.query.13, token = token)
hr.tibble.13 <- as_tibble(hr.response.13, type = "ad")

fb.house <- rbind(fb.house, hr.tibble.13)

# Group 14 =====================================================================

hr.id.14 <- as.vector(houseframes[["14"]]$id)
hr.query.14 <- adlib_build_query(ad_reached_countries = 'US', 
                                 ad_active_status = 'ALL',
                                 search_page_ids = hr.id.14,
                                 ad_delivery_date_max = '2020-11-03',
                                 ad_delivery_date_min = '2020-01-01',
                                 limit = 5000,
                                 fields = "ad_data")
hr.response.14 <- adlib_get(params = hr.query.14, token = token)
hr.tibble.14 <- as_tibble(hr.response.14, type = "ad")

fb.house <- rbind(fb.house, hr.tibble.14)

# Group 15 =====================================================================

hr.id.15 <- as.vector(houseframes[["15"]]$id)
hr.query.15 <- adlib_build_query(ad_reached_countries = 'US', 
                                 ad_active_status = 'ALL',
                                 search_page_ids = hr.id.15,
                                 ad_delivery_date_max = '2020-11-03',
                                 ad_delivery_date_min = '2020-01-01',
                                 limit = 5000,
                                 fields = "ad_data")
hr.response.15 <- adlib_get(params = hr.query.15, token = token)
hr.tibble.15 <- as_tibble(hr.response.15, type = "ad")

fb.house <- rbind(fb.house, hr.tibble.15)

# Group 16 =====================================================================

hr.id.16 <- as.vector(houseframes[["16"]]$id)
hr.query.16 <- adlib_build_query(ad_reached_countries = 'US', 
                                 ad_active_status = 'ALL',
                                 search_page_ids = hr.id.16,
                                 ad_delivery_date_max = '2020-11-03',
                                 ad_delivery_date_min = '2020-01-01',
                                 limit = 5000,
                                 fields = "ad_data")
hr.response.16 <- adlib_get(params = hr.query.16, token = token)
hr.tibble.16 <- as_tibble(hr.response.16, type = "ad")

fb.house <- rbind(fb.house, hr.tibble.16)

# Group 17 =====================================================================

hr.id.17 <- as.vector(houseframes[["17"]]$id)
hr.query.17 <- adlib_build_query(ad_reached_countries = 'US', 
                                 ad_active_status = 'ALL',
                                 search_page_ids = hr.id.17,
                                 ad_delivery_date_max = '2020-11-03',
                                 ad_delivery_date_min = '2020-01-01',
                                 limit = 5000,
                                 fields = "ad_data")
hr.response.17 <- adlib_get(params = hr.query.17, token = token)
hr.tibble.17 <- as_tibble(hr.response.17, type = "ad")

fb.house <- rbind(fb.house, hr.tibble.17)

# Group 18 =====================================================================

hr.id.18 <- as.vector(houseframes[["18"]]$id)
hr.query.18 <- adlib_build_query(ad_reached_countries = 'US', 
                                 ad_active_status = 'ALL',
                                 search_page_ids = hr.id.18,
                                 ad_delivery_date_max = '2020-11-03',
                                 ad_delivery_date_min = '2020-01-01',
                                 limit = 5000,
                                 fields = "ad_data")
hr.response.18 <- adlib_get(params = hr.query.18, token = token)
hr.tibble.18 <- as_tibble(hr.response.18, type = "ad")

fb.house <- rbind(fb.house, hr.tibble.18)

# Group 19 =====================================================================

hr.id.19 <- as.vector(houseframes[["19"]]$id)
hr.query.19 <- adlib_build_query(ad_reached_countries = 'US', 
                                 ad_active_status = 'ALL',
                                 search_page_ids = hr.id.19,
                                 ad_delivery_date_max = '2020-11-03',
                                 ad_delivery_date_min = '2020-01-01',
                                 limit = 5000,
                                 fields = "ad_data")
hr.response.19 <- adlib_get(params = hr.query.19, token = token)
hr.tibble.19 <- as_tibble(hr.response.19, type = "ad")

fb.house <- rbind(fb.house, hr.tibble.19)

# Group 20 =====================================================================

hr.id.20 <- as.vector(houseframes[["20"]]$id)
hr.query.20 <- adlib_build_query(ad_reached_countries = 'US', 
                                 ad_active_status = 'ALL',
                                 search_page_ids = hr.id.20,
                                 ad_delivery_date_max = '2020-11-03',
                                 ad_delivery_date_min = '2020-01-01',
                                 limit = 5000,
                                 fields = "ad_data")
hr.response.20 <- adlib_get(params = hr.query.20, token = token)
hr.tibble.20 <- as_tibble(hr.response.20, type = "ad")

fb.house <- rbind(fb.house, hr.tibble.20)

# Group 21 =====================================================================

hr.id.21 <- as.vector(houseframes[["21"]]$id)
hr.query.21 <- adlib_build_query(ad_reached_countries = 'US', 
                                 ad_active_status = 'ALL',
                                 search_page_ids = hr.id.21,
                                 ad_delivery_date_max = '2020-11-03',
                                 ad_delivery_date_min = '2020-01-01',
                                 limit = 5000,
                                 fields = "ad_data")
hr.response.21 <- adlib_get(params = hr.query.21, token = token)
hr.tibble.21 <- as_tibble(hr.response.21, type = "ad")

fb.house <- rbind(fb.house, hr.tibble.21)

# Group 22 =====================================================================

hr.id.22 <- as.vector(houseframes[["22"]]$id)
hr.query.22 <- adlib_build_query(ad_reached_countries = 'US', 
                                 ad_active_status = 'ALL',
                                 search_page_ids = hr.id.22,
                                 ad_delivery_date_max = '2020-11-03',
                                 ad_delivery_date_min = '2020-01-01',
                                 limit = 5000,
                                 fields = "ad_data")
hr.response.22 <- adlib_get(params = hr.query.22, token = token)
hr.tibble.22 <- as_tibble(hr.response.22, type = "ad")

fb.house <- rbind(fb.house, hr.tibble.22)

# Group 23 =====================================================================

hr.id.23 <- as.vector(houseframes[["23"]]$id)
hr.query.23 <- adlib_build_query(ad_reached_countries = 'US', 
                                 ad_active_status = 'ALL',
                                 search_page_ids = hr.id.23,
                                 ad_delivery_date_max = '2020-11-03',
                                 ad_delivery_date_min = '2020-01-01',
                                 limit = 5000,
                                 fields = "ad_data")
hr.response.23 <- adlib_get(params = hr.query.23, token = token)
hr.tibble.23 <- as_tibble(hr.response.23, type = "ad")

fb.house <- rbind(fb.house, hr.tibble.23)

# Group 24 =====================================================================

hr.id.24 <- as.vector(houseframes[["24"]]$id)
hr.query.24 <- adlib_build_query(ad_reached_countries = 'US', 
                                 ad_active_status = 'ALL',
                                 search_page_ids = hr.id.24,
                                 ad_delivery_date_max = '2020-11-03',
                                 ad_delivery_date_min = '2020-01-01',
                                 limit = 5000,
                                 fields = "ad_data")
hr.response.24 <- adlib_get(params = hr.query.24, token = token)
hr.tibble.24 <- as_tibble(hr.response.24, type = "ad")

fb.house <- rbind(fb.house, hr.tibble.24)

# Group 25 =====================================================================

hr.id.25 <- as.vector(houseframes[["25"]]$id)
hr.query.25 <- adlib_build_query(ad_reached_countries = 'US', 
                                 ad_active_status = 'ALL',
                                 search_page_ids = hr.id.25,
                                 ad_delivery_date_max = '2020-11-03',
                                 ad_delivery_date_min = '2020-01-01',
                                 limit = 5000,
                                 fields = "ad_data")
hr.response.25 <- adlib_get(params = hr.query.25, token = token)
hr.tibble.25 <- as_tibble(hr.response.25, type = "ad")

fb.house <- rbind(fb.house, hr.tibble.25)

# Group 26 =====================================================================

hr.id.26 <- as.vector(houseframes[["26"]]$id)
hr.query.26 <- adlib_build_query(ad_reached_countries = 'US', 
                                 ad_active_status = 'ALL',
                                 search_page_ids = hr.id.26,
                                 ad_delivery_date_max = '2020-11-03',
                                 ad_delivery_date_min = '2020-01-01',
                                 limit = 5000,
                                 fields = "ad_data")
hr.response.26 <- adlib_get(params = hr.query.26, token = token)
hr.tibble.26 <- as_tibble(hr.response.26, type = "ad")

fb.house <- rbind(fb.house, hr.tibble.26)

# Group 27 =====================================================================

hr.id.27 <- as.vector(houseframes[["27"]]$id)
hr.query.27 <- adlib_build_query(ad_reached_countries = 'US', 
                                 ad_active_status = 'ALL',
                                 search_page_ids = hr.id.27,
                                 ad_delivery_date_max = '2020-11-03',
                                 ad_delivery_date_min = '2020-01-01',
                                 limit = 5000,
                                 fields = "ad_data")
hr.response.27 <- adlib_get(params = hr.query.27, token = token)
hr.tibble.27 <- as_tibble(hr.response.27, type = "ad")

fb.house <- rbind(fb.house, hr.tibble.27)

# Group 28 =====================================================================

hr.id.28 <- as.vector(houseframes[["28"]]$id)
hr.query.28 <- adlib_build_query(ad_reached_countries = 'US', 
                                 ad_active_status = 'ALL',
                                 search_page_ids = hr.id.28,
                                 ad_delivery_date_max = '2020-11-03',
                                 ad_delivery_date_min = '2020-01-01',
                                 limit = 5000,
                                 fields = "ad_data")
hr.response.28 <- adlib_get(params = hr.query.28, token = token)
hr.tibble.28 <- as_tibble(hr.response.28, type = "ad")

fb.house <- rbind(fb.house, hr.tibble.28)

# Group 29 =====================================================================

hr.id.29 <- as.vector(houseframes[["29"]]$id)
hr.query.29 <- adlib_build_query(ad_reached_countries = 'US', 
                                 ad_active_status = 'ALL',
                                 search_page_ids = hr.id.29,
                                 ad_delivery_date_max = '2020-11-03',
                                 ad_delivery_date_min = '2020-01-01',
                                 limit = 5000,
                                 fields = "ad_data")
hr.response.29 <- adlib_get(params = hr.query.29, token = token)
hr.tibble.29 <- as_tibble(hr.response.29, type = "ad")

fb.house <- rbind(fb.house, hr.tibble.29)

# Group 30 =====================================================================

hr.id.30 <- as.vector(houseframes[["30"]]$id)
hr.query.30 <- adlib_build_query(ad_reached_countries = 'US', 
                                 ad_active_status = 'ALL',
                                 search_page_ids = hr.id.30,
                                 ad_delivery_date_max = '2020-11-03',
                                 ad_delivery_date_min = '2020-01-01',
                                 limit = 5000,
                                 fields = "ad_data")
hr.response.30 <- adlib_get(params = hr.query.30, token = token)
hr.tibble.30 <- as_tibble(hr.response.30, type = "ad")

fb.house <- rbind(fb.house, hr.tibble.30)

# Group 31 =====================================================================

hr.id.31 <- as.vector(houseframes[["31"]]$id)
hr.query.31 <- adlib_build_query(ad_reached_countries = 'US', 
                                 ad_active_status = 'ALL',
                                 search_page_ids = hr.id.31,
                                 ad_delivery_date_max = '2020-11-03',
                                 ad_delivery_date_min = '2020-01-01',
                                 limit = 5000,
                                 fields = "ad_data")
hr.response.31 <- adlib_get(params = hr.query.31, token = token)
hr.tibble.31 <- as_tibble(hr.response.31, type = "ad")

fb.house <- rbind(fb.house, hr.tibble.31)

# Group 32 =====================================================================

hr.id.32 <- as.vector(houseframes[["32"]]$id)
hr.query.32 <- adlib_build_query(ad_reached_countries = 'US', 
                                 ad_active_status = 'ALL',
                                 search_page_ids = hr.id.32,
                                 ad_delivery_date_max = '2020-11-03',
                                 ad_delivery_date_min = '2020-01-01',
                                 limit = 5000,
                                 fields = "ad_data")
hr.response.32 <- adlib_get(params = hr.query.32, token = token)
hr.tibble.32 <- as_tibble(hr.response.32, type = "ad")

fb.house <- rbind(fb.house, hr.tibble.32)

# Group 33 =====================================================================

hr.id.33 <- as.vector(houseframes[["33"]]$id)
hr.query.33 <- adlib_build_query(ad_reached_countries = 'US', 
                                 ad_active_status = 'ALL',
                                 search_page_ids = hr.id.33,
                                 ad_delivery_date_max = '2020-11-03',
                                 ad_delivery_date_min = '2020-01-01',
                                 limit = 5000,
                                 fields = "ad_data")
hr.response.33 <- adlib_get(params = hr.query.33, token = token)
hr.tibble.33 <- as_tibble(hr.response.33, type = "ad")

fb.house <- rbind(fb.house, hr.tibble.33)

# Group 34 =====================================================================

hr.id.34 <- as.vector(houseframes[["34"]]$id)
hr.query.34 <- adlib_build_query(ad_reached_countries = 'US', 
                                 ad_active_status = 'ALL',
                                 search_page_ids = hr.id.34,
                                 ad_delivery_date_max = '2020-11-03',
                                 ad_delivery_date_min = '2020-01-01',
                                 limit = 5000,
                                 fields = "ad_data")
hr.response.34 <- adlib_get(params = hr.query.34, token = token)
hr.tibble.34 <- as_tibble(hr.response.34, type = "ad")

fb.house <- rbind(fb.house, hr.tibble.34)

# Group 35 =====================================================================

hr.id.35 <- as.vector(houseframes[["35"]]$id)
hr.query.35 <- adlib_build_query(ad_reached_countries = 'US', 
                                 ad_active_status = 'ALL',
                                 search_page_ids = hr.id.35,
                                 ad_delivery_date_max = '2020-11-03',
                                 ad_delivery_date_min = '2020-01-01',
                                 limit = 5000,
                                 fields = "ad_data")
hr.response.35 <- adlib_get(params = hr.query.35, token = token)
hr.tibble.35 <- as_tibble(hr.response.35, type = "ad")

fb.house <- rbind(fb.house, hr.tibble.35)

# Group 36 =====================================================================

hr.id.36 <- as.vector(houseframes[["36"]]$id)
hr.query.36 <- adlib_build_query(ad_reached_countries = 'US', 
                                 ad_active_status = 'ALL',
                                 search_page_ids = hr.id.36,
                                 ad_delivery_date_max = '2020-11-03',
                                 ad_delivery_date_min = '2020-01-01',
                                 limit = 5000,
                                 fields = "ad_data")
hr.response.36 <- adlib_get(params = hr.query.36, token = token)
hr.tibble.36 <- as_tibble(hr.response.36, type = "ad")

fb.house <- rbind(fb.house, hr.tibble.36)

# Group 37 =====================================================================

hr.id.37 <- as.vector(houseframes[["37"]]$id)
hr.query.37 <- adlib_build_query(ad_reached_countries = 'US', 
                                 ad_active_status = 'ALL',
                                 search_page_ids = hr.id.37,
                                 ad_delivery_date_max = '2020-11-03',
                                 ad_delivery_date_min = '2020-01-01',
                                 limit = 5000,
                                 fields = "ad_data")
hr.response.37 <- adlib_get(params = hr.query.37, token = token)
hr.tibble.37 <- as_tibble(hr.response.37, type = "ad")

fb.house <- rbind(fb.house, hr.tibble.37)

# Group 38 =====================================================================

hr.id.38 <- as.vector(houseframes[["38"]]$id)
hr.query.38 <- adlib_build_query(ad_reached_countries = 'US', 
                                 ad_active_status = 'ALL',
                                 search_page_ids = hr.id.38,
                                 ad_delivery_date_max = '2020-11-03',
                                 ad_delivery_date_min = '2020-01-01',
                                 limit = 5000,
                                 fields = "ad_data")
hr.response.38 <- adlib_get(params = hr.query.38, token = token)
hr.tibble.38 <- as_tibble(hr.response.38, type = "ad")

fb.house <- rbind(fb.house, hr.tibble.38)

# Group 39 =====================================================================

hr.id.39 <- as.vector(houseframes[["39"]]$id)
hr.query.39 <- adlib_build_query(ad_reached_countries = 'US', 
                                 ad_active_status = 'ALL',
                                 search_page_ids = hr.id.39,
                                 ad_delivery_date_max = '2020-11-03',
                                 ad_delivery_date_min = '2020-01-01',
                                 limit = 5000,
                                 fields = "ad_data")
hr.response.39 <- adlib_get(params = hr.query.39, token = token)
hr.tibble.39 <- as_tibble(hr.response.39, type = "ad")

fb.house <- rbind(fb.house, hr.tibble.39)

# Group 40 =====================================================================

hr.id.40 <- as.vector(houseframes[["40"]]$id)
hr.query.40 <- adlib_build_query(ad_reached_countries = 'US', 
                                 ad_active_status = 'ALL',
                                 search_page_ids = hr.id.40,
                                 ad_delivery_date_max = '2020-11-03',
                                 ad_delivery_date_min = '2020-01-01',
                                 limit = 5000,
                                 fields = "ad_data")
hr.response.40 <- adlib_get(params = hr.query.40, token = token)
hr.tibble.40 <- as_tibble(hr.response.40, type = "ad")

fb.house <- rbind(fb.house, hr.tibble.40)

# Group 41 =====================================================================

hr.id.41 <- as.vector(houseframes[["41"]]$id)
hr.query.41 <- adlib_build_query(ad_reached_countries = 'US', 
                                 ad_active_status = 'ALL',
                                 search_page_ids = hr.id.41,
                                 ad_delivery_date_max = '2020-11-03',
                                 ad_delivery_date_min = '2020-01-01',
                                 limit = 5000,
                                 fields = "ad_data")
hr.response.41 <- adlib_get(params = hr.query.41, token = token)
hr.tibble.41 <- as_tibble(hr.response.41, type = "ad")

fb.house <- rbind(fb.house, hr.tibble.41)

# Group 42 =====================================================================

hr.id.42 <- as.vector(houseframes[["42"]]$id)
hr.query.42 <- adlib_build_query(ad_reached_countries = 'US', 
                                 ad_active_status = 'ALL',
                                 search_page_ids = hr.id.42,
                                 ad_delivery_date_max = '2020-11-03',
                                 ad_delivery_date_min = '2020-01-01',
                                 limit = 5000,
                                 fields = "ad_data")
hr.response.42 <- adlib_get(params = hr.query.42, token = token)
hr.tibble.42 <- as_tibble(hr.response.42, type = "ad")

fb.house <- rbind(fb.house, hr.tibble.42)

# Group 43 =====================================================================

## Something is bugged here...

hr.id.43 <- as.vector(houseframes[["43"]]$id)
hr.query.43 <- adlib_build_query(ad_reached_countries = 'US', 
                                 ad_active_status = 'ALL',
                                 search_page_ids = hr.id.43,
                                 ad_delivery_date_max = '2020-11-03',
                                 ad_delivery_date_min = '2020-01-01',
                                 limit = 5000,
                                 fields = "ad_data")
hr.response.43 <- adlib_get(params = hr.query.43, token = token)
hr.tibble.43 <- as_tibble(hr.response.43, type = "ad")

## For some reason, exactly the same code structure and type of variables yield
## an error here, whereas they haven't done so anywhere else

### Error in extract_error_message(response) : HTTP Error:
### {
###   "error": {
###     "message": "(#100) Param search_page_ids[8] must be an integer. Instead, got float.",
###     "type": "OAuthException",
###     "code": 100,
###     "fbtrace_id": "AYiZuqCxMZuMp8fcyvgLJWp"
###   }
### }
fb.house <- rbind(fb.house, hr.tibble.43)

## Something is very wrong with this...

## Piecemeal: 43.1 =============================================================
hr.id.43.1 <- hr.id.43[1]
hr.query.43.1 <- adlib_build_query(ad_reached_countries = 'US', 
                                 ad_active_status = 'ALL',
                                 search_page_ids = hr.id.43.1,
                                 ad_delivery_date_max = '2020-11-03',
                                 ad_delivery_date_min = '2020-01-01',
                                 limit = 5000,
                                 fields = "ad_data")
hr.response.43.1 <- adlib_get(params = hr.query.43.1, token = token)
hr.tibble.43.1 <- as_tibble(hr.response.43.1, type = "ad")
hr.tibble.43 <- hr.tibble.43.1
## Piecemeal: 43.2 =============================================================
hr.id.43.2 <- hr.id.43[2]
hr.query.43.2 <- adlib_build_query(ad_reached_countries = 'US', 
                                   ad_active_status = 'ALL',
                                   search_page_ids = hr.id.43.2,
                                   ad_delivery_date_max = '2020-11-03',
                                   ad_delivery_date_min = '2020-01-01',
                                   limit = 5000,
                                   fields = "ad_data")
hr.response.43.2 <- adlib_get(params = hr.query.43.2, token = token)
hr.tibble.43.2 <- as_tibble(hr.response.43.2, type = "ad")
hr.tibble.43 <- rbind(hr.tibble.43, hr.tibble.43.2)
## Piecemeal: 43.3 =============================================================
hr.id.43.3 <- hr.id.43[3]
hr.query.43.3 <- adlib_build_query(ad_reached_countries = 'US', 
                                   ad_active_status = 'ALL',
                                   search_page_ids = hr.id.43.3,
                                   ad_delivery_date_max = '2020-11-03',
                                   ad_delivery_date_min = '2020-01-01',
                                   limit = 5000,
                                   fields = "ad_data")
hr.response.43.3 <- adlib_get(params = hr.query.43.3, token = token)
hr.tibble.43.3 <- as_tibble(hr.response.43.3, type = "ad")
hr.tibble.43 <- rbind(hr.tibble.43, hr.tibble.43.3)
## Piecemeal: 43.4 =============================================================
hr.id.43.4 <- hr.id.43[4]
hr.query.43.4 <- adlib_build_query(ad_reached_countries = 'US', 
                                   ad_active_status = 'ALL',
                                   search_page_ids = hr.id.43.4,
                                   ad_delivery_date_max = '2020-11-03',
                                   ad_delivery_date_min = '2020-01-01',
                                   limit = 5000,
                                   fields = "ad_data")
hr.response.43.4 <- adlib_get(params = hr.query.43.4, token = token)
hr.tibble.43.4 <- as_tibble(hr.response.43.4, type = "ad")
hr.tibble.43 <- rbind(hr.tibble.43, hr.tibble.43.4)
## Piecemeal: 43.5 =============================================================
hr.id.43.5 <- hr.id.43[5]
hr.query.43.5 <- adlib_build_query(ad_reached_countries = 'US', 
                                   ad_active_status = 'ALL',
                                   search_page_ids = hr.id.43.5,
                                   ad_delivery_date_max = '2020-11-03',
                                   ad_delivery_date_min = '2020-01-01',
                                   limit = 5000,
                                   fields = "ad_data")
hr.response.43.5 <- adlib_get(params = hr.query.43.5, token = token)
hr.tibble.43.5 <- as_tibble(hr.response.43.5, type = "ad")
hr.tibble.43 <- rbind(hr.tibble.43, hr.tibble.43.5)
## Piecemeal: 43.6 =============================================================
hr.id.43.6 <- hr.id.43[6]
hr.query.43.6 <- adlib_build_query(ad_reached_countries = 'US', 
                                   ad_active_status = 'ALL',
                                   search_page_ids = hr.id.43.6,
                                   ad_delivery_date_max = '2020-11-03',
                                   ad_delivery_date_min = '2020-01-01',
                                   limit = 5000,
                                   fields = "ad_data")
hr.response.43.6 <- adlib_get(params = hr.query.43.6, token = token)
hr.tibble.43.6 <- as_tibble(hr.response.43.6, type = "ad")
hr.tibble.43 <- rbind(hr.tibble.43, hr.tibble.43.6)
## Piecemeal: 43.7 =============================================================
hr.id.43.7 <- hr.id.43[7]
hr.query.43.7 <- adlib_build_query(ad_reached_countries = 'US', 
                                   ad_active_status = 'ALL',
                                   search_page_ids = hr.id.43.7,
                                   ad_delivery_date_max = '2020-11-03',
                                   ad_delivery_date_min = '2020-01-01',
                                   limit = 5000,
                                   fields = "ad_data")
hr.response.43.7 <- adlib_get(params = hr.query.43.7, token = token)
hr.tibble.43.7 <- as_tibble(hr.response.43.7, type = "ad")
hr.tibble.43 <- rbind(hr.tibble.43, hr.tibble.43.7)
## Piecemeal: 43.8 =============================================================
hr.id.43.8 <- hr.id.43[8]
hr.query.43.8 <- adlib_build_query(ad_reached_countries = 'US', 
                                   ad_active_status = 'ALL',
                                   search_page_ids = hr.id.43.8,
                                   ad_delivery_date_max = '2020-11-03',
                                   ad_delivery_date_min = '2020-01-01',
                                   limit = 5000,
                                   fields = "ad_data")
hr.response.43.8 <- adlib_get(params = hr.query.43.8, token = token)
hr.tibble.43.8 <- as_tibble(hr.response.43.8, type = "ad")
hr.tibble.43 <- rbind(hr.tibble.43, hr.tibble.43.8)
## Piecemeal: 43.9 =============================================================
#hr.id.43.9 <- hr.id.43[9]
#hr.query.43.9 <- adlib_build_query(ad_reached_countries = 'US', 
#                                   ad_active_status = 'ALL',
#                                   search_page_ids = hr.id.43.9,
#                                   ad_delivery_date_max = '2020-11-03',
#                                   ad_delivery_date_min = '2020-01-01',
#                                   limit = 5000,
#                                   fields = "ad_data")
#hr.response.43.9 <- adlib_get(params = hr.query.43.9, token = token)
#hr.tibble.43.9 <- as_tibble(hr.response.43.9, type = "ad")
#hr.tibble.43 <- rbind(hr.tibble.43, hr.tibble.43.9)

## This one is the source of the error.
## The candidate in question is Yvette Herrell.
## Piecemeal: 43.10 =============================================================
hr.id.43.10 <- hr.id.43[10]
hr.query.43.10 <- adlib_build_query(ad_reached_countries = 'US', 
                                   ad_active_status = 'ALL',
                                   search_page_ids = hr.id.43.10,
                                   ad_delivery_date_max = '2020-11-03',
                                   ad_delivery_date_min = '2020-01-01',
                                   limit = 5000,
                                   fields = "ad_data")
hr.response.43.10 <- adlib_get(params = hr.query.43.10, token = token)
hr.tibble.43.10 <- as_tibble(hr.response.43.10, type = "ad")
hr.tibble.43 <- rbind(hr.tibble.43, hr.tibble.43.10)

fb.house <- rbind(fb.house, hr.tibble.43)

# Notes on the error here ======================================================

# Having run into the same issue later on (in frame 50), with the benefit of
# hindsight and having done all of this, I now know what was going on. For
# whatever reason, Radlibrary can't handle any IDs that are longer than
# 15 digits. I'm going to make a note of the candidates who could not be coded
# at the top of this document.

# Group 44 =====================================================================

hr.id.44 <- as.vector(houseframes[["44"]]$id)
hr.query.44 <- adlib_build_query(ad_reached_countries = 'US', 
                                 ad_active_status = 'ALL',
                                 search_page_ids = hr.id.44,
                                 ad_delivery_date_max = '2020-11-03',
                                 ad_delivery_date_min = '2020-01-01',
                                 limit = 5000,
                                 fields = "ad_data")
hr.response.44 <- adlib_get(params = hr.query.44, token = token)
hr.tibble.44 <- as_tibble(hr.response.44, type = "ad")
fb.house <- rbind(fb.house, hr.tibble.44)

# Group 45 =====================================================================

hr.id.45 <- as.vector(houseframes[["45"]]$id)
hr.query.45 <- adlib_build_query(ad_reached_countries = 'US', 
                                 ad_active_status = 'ALL',
                                 search_page_ids = hr.id.45,
                                 ad_delivery_date_max = '2020-11-03',
                                 ad_delivery_date_min = '2020-01-01',
                                 limit = 5000,
                                 fields = "ad_data")
hr.response.45 <- adlib_get(params = hr.query.45, token = token)
hr.tibble.45 <- as_tibble(hr.response.45, type = "ad")
fb.house <- rbind(fb.house, hr.tibble.45)

# Group 46 =====================================================================

hr.id.46 <- as.vector(houseframes[["46"]]$id)
hr.query.46 <- adlib_build_query(ad_reached_countries = 'US', 
                                 ad_active_status = 'ALL',
                                 search_page_ids = hr.id.46,
                                 ad_delivery_date_max = '2020-11-03',
                                 ad_delivery_date_min = '2020-01-01',
                                 limit = 5000,
                                 fields = "ad_data")
hr.response.46 <- adlib_get(params = hr.query.46, token = token)
hr.tibble.46 <- as_tibble(hr.response.46, type = "ad")
fb.house <- rbind(fb.house, hr.tibble.46)

# Group 47 =====================================================================

hr.id.47 <- as.vector(houseframes[["47"]]$id)
hr.query.47 <- adlib_build_query(ad_reached_countries = 'US', 
                                 ad_active_status = 'ALL',
                                 search_page_ids = hr.id.47,
                                 ad_delivery_date_max = '2020-11-03',
                                 ad_delivery_date_min = '2020-01-01',
                                 limit = 5000,
                                 fields = "ad_data")
hr.response.47 <- adlib_get(params = hr.query.47, token = token)
hr.tibble.47 <- as_tibble(hr.response.47, type = "ad")
fb.house <- rbind(fb.house, hr.tibble.47)

# Group 48 =====================================================================

hr.id.48 <- as.vector(houseframes[["48"]]$id)
hr.query.48 <- adlib_build_query(ad_reached_countries = 'US', 
                                 ad_active_status = 'ALL',
                                 search_page_ids = hr.id.48,
                                 ad_delivery_date_max = '2020-11-03',
                                 ad_delivery_date_min = '2020-01-01',
                                 limit = 5000,
                                 fields = "ad_data")
hr.response.48 <- adlib_get(params = hr.query.48, token = token)
hr.tibble.48 <- as_tibble(hr.response.48, type = "ad")
# This one breaks 5,000...Darn it, AOC, I had a good streak going here
# Here's what we're going to do: we're going to pull one query for everyone
# but Alexandria Ocasio-Cortez, and then we'll do one (or more) for AOC
# specifically.
hr.id.48.notaoc <- hr.id.48[-3]
hr.query.48.notaoc <- adlib_build_query(ad_reached_countries = 'US', 
                                 ad_active_status = 'ALL',
                                 search_page_ids = hr.id.48.notaoc,
                                 ad_delivery_date_max = '2020-11-03',
                                 ad_delivery_date_min = '2020-01-01',
                                 limit = 5000,
                                 fields = "ad_data")
hr.response.48.notaoc <- adlib_get(params = hr.query.48.notaoc, token = token)
hr.tibble.48.notaoc <- as_tibble(hr.response.48.notaoc, type = "ad")

hr.id.48.aoc <- hr.id.48[3]
hr.query.48.aoc <- adlib_build_query(ad_reached_countries = 'US', 
                                        ad_active_status = 'ALL',
                                        search_page_ids = hr.id.48.aoc,
                                        ad_delivery_date_max = '2020-11-03',
                                        ad_delivery_date_min = '2020-01-01',
                                        limit = 5000,
                                        fields = "ad_data")
hr.response.48.aoc <- adlib_get(params = hr.query.48.aoc, token = token)
hr.tibble.48.aoc <- as_tibble(hr.response.48.aoc, type = "ad")
# Jeez. A tad excessive, methinks. Time for round two.
hr.query.48.aoc.2 <- adlib_build_query(ad_reached_countries = 'US', 
                                     ad_active_status = 'ALL',
                                     search_page_ids = hr.id.48.aoc,
                                     ad_delivery_date_max = '2020-10-01',
                                     ad_delivery_date_min = '2020-01-01',
                                     limit = 5000,
                                     fields = "ad_data")
hr.response.48.aoc.2 <- adlib_get(params = hr.query.48.aoc.2, token = token)
hr.tibble.48.aoc.2 <- as_tibble(hr.response.48.aoc.2, type = "ad")
# More than a tad excessive. Round three.
hr.query.48.aoc.3 <- adlib_build_query(ad_reached_countries = 'US', 
                                       ad_active_status = 'ALL',
                                       search_page_ids = hr.id.48.aoc,
                                       ad_delivery_date_max = '2020-09-01',
                                       ad_delivery_date_min = '2020-01-01',
                                       limit = 5000,
                                       fields = "ad_data")
hr.response.48.aoc.3 <- adlib_get(params = hr.query.48.aoc.3, token = token)
hr.tibble.48.aoc.3 <- as_tibble(hr.response.48.aoc.3, type = "ad")
# Well, there's definitely lots of data here...round four.
hr.query.48.aoc.4 <- adlib_build_query(ad_reached_countries = 'US', 
                                       ad_active_status = 'ALL',
                                       search_page_ids = hr.id.48.aoc,
                                       ad_delivery_date_max = '2020-07-30',
                                       ad_delivery_date_min = '2020-01-01',
                                       limit = 5000,
                                       fields = "ad_data")
hr.response.48.aoc.4 <- adlib_get(params = hr.query.48.aoc.4, token = token)
hr.tibble.48.aoc.4 <- as_tibble(hr.response.48.aoc.4, type = "ad")
# Round five.
hr.query.48.aoc.5 <- adlib_build_query(ad_reached_countries = 'US', 
                                       ad_active_status = 'ALL',
                                       search_page_ids = hr.id.48.aoc,
                                       ad_delivery_date_max = '2020-06-20',
                                       ad_delivery_date_min = '2020-01-01',
                                       limit = 5000,
                                       fields = "ad_data")
hr.response.48.aoc.5 <- adlib_get(params = hr.query.48.aoc.5, token = token)
hr.tibble.48.aoc.5 <- as_tibble(hr.response.48.aoc.5, type = "ad")
# Round six
hr.query.48.aoc.6 <- adlib_build_query(ad_reached_countries = 'US', 
                                       ad_active_status = 'ALL',
                                       search_page_ids = hr.id.48.aoc,
                                       ad_delivery_date_max = '2020-05-15',
                                       ad_delivery_date_min = '2020-01-01',
                                       limit = 5000,
                                       fields = "ad_data")
hr.response.48.aoc.6 <- adlib_get(params = hr.query.48.aoc.6, token = token)
hr.tibble.48.aoc.6 <- as_tibble(hr.response.48.aoc.6, type = "ad")
# Round seven
hr.query.48.aoc.7 <- adlib_build_query(ad_reached_countries = 'US', 
                                       ad_active_status = 'ALL',
                                       search_page_ids = hr.id.48.aoc,
                                       ad_delivery_date_max = '2020-02-15',
                                       ad_delivery_date_min = '2020-01-01',
                                       limit = 5000,
                                       fields = "ad_data")
hr.response.48.aoc.7 <- adlib_get(params = hr.query.48.aoc.7, token = token)
hr.tibble.48.aoc.7 <- as_tibble(hr.response.48.aoc.7, type = "ad")

hr.tibble.aoc <- rbind (hr.tibble.48.aoc, hr.tibble.48.aoc.2, 
                        hr.tibble.48.aoc.3, hr.tibble.48.aoc.4, 
                        hr.tibble.48.aoc.5, hr.tibble.48.aoc.6, 
                        hr.tibble.48.aoc.7)
#library(dplyr)
aoc.distinct <- distinct(hr.tibble.aoc)
hr.tibble.48 <- rbind(hr.tibble.48.notaoc, aoc.distinct)

fb.house <- rbind(fb.house, hr.tibble.48)

# Group 49 =====================================================================

hr.id.49 <- as.vector(houseframes[["49"]]$id)
hr.query.49 <- adlib_build_query(ad_reached_countries = 'US', 
                                 ad_active_status = 'ALL',
                                 search_page_ids = hr.id.49,
                                 ad_delivery_date_max = '2020-11-03',
                                 ad_delivery_date_min = '2020-01-01',
                                 limit = 5000,
                                 fields = "ad_data")
hr.response.49 <- adlib_get(params = hr.query.49, token = token)
hr.tibble.49 <- as_tibble(hr.response.49, type = "ad")
fb.house <- rbind(fb.house, hr.tibble.49)

# Group 50 =====================================================================

hr.id.50 <- as.vector(houseframes[["50"]]$id)
#hr.query.50 <- adlib_build_query(ad_reached_countries = 'US', 
#                                 ad_active_status = 'ALL',
#                                 search_page_ids = hr.id.50,
#                                 ad_delivery_date_max = '2020-11-03',
#                                 ad_delivery_date_min = '2020-01-01',
#                                 limit = 5000,
#                                 fields = "ad_data")
#hr.response.50 <- adlib_get(params = hr.query.50, token = token)
#hr.tibble.50 <- as_tibble(hr.response.50, type = "ad")
#fb.house <- rbind(fb.house, hr.tibble.50)
# Got the same error as with 43...but I might have an idea of what's going on.
hr.id.50.less6 <- hr.id.50[-7]
hr.query.50 <- adlib_build_query(ad_reached_countries = 'US', 
                                 ad_active_status = 'ALL',
                                 search_page_ids = hr.id.50.less6,
                                 ad_delivery_date_max = '2020-11-03',
                                 ad_delivery_date_min = '2020-01-01',
                                 limit = 5000,
                                 fields = "ad_data")
hr.response.50 <- adlib_get(params = hr.query.50, token = token)
hr.tibble.50 <- as_tibble(hr.response.50, type = "ad")

# I've figured out the issue at hand now, I think.
# For whatever reason, Radlibrary can't handle page IDs that are longer than
# 15 digits.
# The excluded candidate is Ted Budd.

# Group 51 =====================================================================

hr.id.51 <- as.vector(houseframes[["51"]]$id)
hr.query.51 <- adlib_build_query(ad_reached_countries = 'US', 
                                 ad_active_status = 'ALL',
                                 search_page_ids = hr.id.51,
                                 ad_delivery_date_max = '2020-11-03',
                                 ad_delivery_date_min = '2020-01-01',
                                 limit = 5000,
                                 fields = "ad_data")
hr.response.51 <- adlib_get(params = hr.query.51, token = token)
hr.tibble.51 <- as_tibble(hr.response.51, type = "ad")
fb.house <- rbind(fb.house, hr.tibble.51)

# Group 52 =====================================================================

hr.id.52 <- as.vector(houseframes[["52"]]$id)
hr.query.52 <- adlib_build_query(ad_reached_countries = 'US', 
                                 ad_active_status = 'ALL',
                                 search_page_ids = hr.id.52,
                                 ad_delivery_date_max = '2020-11-03',
                                 ad_delivery_date_min = '2020-01-01',
                                 limit = 5000,
                                 fields = "ad_data")
hr.response.52 <- adlib_get(params = hr.query.52, token = token)
hr.tibble.52 <- as_tibble(hr.response.52, type = "ad")
fb.house <- rbind(fb.house, hr.tibble.52)

# Group 53 =====================================================================

hr.id.53 <- as.vector(houseframes[["53"]]$id)
hr.query.53 <- adlib_build_query(ad_reached_countries = 'US', 
                                 ad_active_status = 'ALL',
                                 search_page_ids = hr.id.53,
                                 ad_delivery_date_max = '2020-11-03',
                                 ad_delivery_date_min = '2020-01-01',
                                 limit = 5000,
                                 fields = "ad_data")
hr.response.53 <- adlib_get(params = hr.query.53, token = token)
hr.tibble.53 <- as_tibble(hr.response.53, type = "ad")
fb.house <- rbind(fb.house, hr.tibble.53)

# Group 54 =====================================================================

hr.id.54 <- as.vector(houseframes[["54"]]$id)
hr.query.54 <- adlib_build_query(ad_reached_countries = 'US', 
                                 ad_active_status = 'ALL',
                                 search_page_ids = hr.id.54,
                                 ad_delivery_date_max = '2020-11-03',
                                 ad_delivery_date_min = '2020-01-01',
                                 limit = 5000,
                                 fields = "ad_data")
hr.response.54 <- adlib_get(params = hr.query.54, token = token)
hr.tibble.54 <- as_tibble(hr.response.54, type = "ad")
fb.house <- rbind(fb.house, hr.tibble.54)

# Group 55 =====================================================================

hr.id.55 <- as.vector(houseframes[["55"]]$id)
hr.query.55 <- adlib_build_query(ad_reached_countries = 'US', 
                                 ad_active_status = 'ALL',
                                 search_page_ids = hr.id.55,
                                 ad_delivery_date_max = '2020-11-03',
                                 ad_delivery_date_min = '2020-01-01',
                                 limit = 5000,
                                 fields = "ad_data")
hr.response.55 <- adlib_get(params = hr.query.55, token = token)
hr.tibble.55 <- as_tibble(hr.response.55, type = "ad")
fb.house <- rbind(fb.house, hr.tibble.55)

# This one is just over 5,000...needs a part 2.
hr.query.55b <- adlib_build_query(ad_reached_countries = 'US', 
                                 ad_active_status = 'ALL',
                                 search_page_ids = hr.id.55,
                                 ad_delivery_date_max = '2020-02-01',
                                 ad_delivery_date_min = '2020-01-01',
                                 limit = 5000,
                                 fields = "ad_data")
hr.response.55b <- adlib_get(params = hr.query.55b, token = token)
hr.tibble.55b <- as_tibble(hr.response.55b, type = "ad")
fb.house <- rbind(fb.house, hr.tibble.55b)

# Group 56 =====================================================================

hr.id.56 <- as.vector(houseframes[["56"]]$id)
hr.query.56 <- adlib_build_query(ad_reached_countries = 'US', 
                                 ad_active_status = 'ALL',
                                 search_page_ids = hr.id.56,
                                 ad_delivery_date_max = '2020-11-03',
                                 ad_delivery_date_min = '2020-01-01',
                                 limit = 5000,
                                 fields = "ad_data")
hr.response.56 <- adlib_get(params = hr.query.56, token = token)
hr.tibble.56 <- as_tibble(hr.response.56, type = "ad")
fb.house <- rbind(fb.house, hr.tibble.56)

# Group 57 =====================================================================

hr.id.57 <- as.vector(houseframes[["57"]]$id)
hr.query.57 <- adlib_build_query(ad_reached_countries = 'US', 
                                 ad_active_status = 'ALL',
                                 search_page_ids = hr.id.57,
                                 ad_delivery_date_max = '2020-11-03',
                                 ad_delivery_date_min = '2020-01-01',
                                 limit = 5000,
                                 fields = "ad_data")
hr.response.57 <- adlib_get(params = hr.query.57, token = token)
hr.tibble.57 <- as_tibble(hr.response.57, type = "ad")
fb.house <- rbind(fb.house, hr.tibble.57)

# Group 58 =====================================================================

hr.id.58 <- as.vector(houseframes[["58"]]$id)
hr.query.58 <- adlib_build_query(ad_reached_countries = 'US', 
                                 ad_active_status = 'ALL',
                                 search_page_ids = hr.id.58,
                                 ad_delivery_date_max = '2020-11-03',
                                 ad_delivery_date_min = '2020-01-01',
                                 limit = 5000,
                                 fields = "ad_data")
hr.response.58 <- adlib_get(params = hr.query.58, token = token)
hr.tibble.58 <- as_tibble(hr.response.58, type = "ad")
fb.house <- rbind(fb.house, hr.tibble.58)

# Group 59 =====================================================================

hr.id.59 <- as.vector(houseframes[["59"]]$id)
hr.query.59 <- adlib_build_query(ad_reached_countries = 'US', 
                                 ad_active_status = 'ALL',
                                 search_page_ids = hr.id.59,
                                 ad_delivery_date_max = '2020-11-03',
                                 ad_delivery_date_min = '2020-01-01',
                                 limit = 5000,
                                 fields = "ad_data")
hr.response.59 <- adlib_get(params = hr.query.59, token = token)
hr.tibble.59 <- as_tibble(hr.response.59, type = "ad")
fb.house <- rbind(fb.house, hr.tibble.59)

# Group 60 =====================================================================

hr.id.60 <- as.vector(houseframes[["60"]]$id)
hr.query.60 <- adlib_build_query(ad_reached_countries = 'US', 
                                 ad_active_status = 'ALL',
                                 search_page_ids = hr.id.60,
                                 ad_delivery_date_max = '2020-11-03',
                                 ad_delivery_date_min = '2020-01-01',
                                 limit = 5000,
                                 fields = "ad_data")
hr.response.60 <- adlib_get(params = hr.query.60, token = token)
hr.tibble.60 <- as_tibble(hr.response.60, type = "ad")
fb.house <- rbind(fb.house, hr.tibble.60)

# Group 61 =====================================================================

hr.id.61 <- as.vector(houseframes[["61"]]$id)
hr.query.61 <- adlib_build_query(ad_reached_countries = 'US', 
                                 ad_active_status = 'ALL',
                                 search_page_ids = hr.id.61,
                                 ad_delivery_date_max = '2020-11-03',
                                 ad_delivery_date_min = '2020-01-01',
                                 limit = 5000,
                                 fields = "ad_data")
hr.response.61 <- adlib_get(params = hr.query.61, token = token)
hr.tibble.61 <- as_tibble(hr.response.61, type = "ad")
fb.house <- rbind(fb.house, hr.tibble.61)

# Group 62 =====================================================================

hr.id.62 <- as.vector(houseframes[["62"]]$id)
hr.query.62 <- adlib_build_query(ad_reached_countries = 'US', 
                                 ad_active_status = 'ALL',
                                 search_page_ids = hr.id.62,
                                 ad_delivery_date_max = '2020-11-03',
                                 ad_delivery_date_min = '2020-01-01',
                                 limit = 5000,
                                 fields = "ad_data")
hr.response.62 <- adlib_get(params = hr.query.62, token = token)
hr.tibble.62 <- as_tibble(hr.response.62, type = "ad")
fb.house <- rbind(fb.house, hr.tibble.62)

# Group 63 =====================================================================

hr.id.63 <- as.vector(houseframes[["63"]]$id)
hr.query.63 <- adlib_build_query(ad_reached_countries = 'US', 
                                 ad_active_status = 'ALL',
                                 search_page_ids = hr.id.63,
                                 ad_delivery_date_max = '2020-11-03',
                                 ad_delivery_date_min = '2020-01-01',
                                 limit = 5000,
                                 fields = "ad_data")
hr.response.63 <- adlib_get(params = hr.query.63, token = token)
hr.tibble.63 <- as_tibble(hr.response.63, type = "ad")
fb.house <- rbind(fb.house, hr.tibble.63)

# Group 64 =====================================================================

hr.id.64 <- as.vector(houseframes[["64"]]$id)
#hr.query.64 <- adlib_build_query(ad_reached_countries = 'US', 
#                                 ad_active_status = 'ALL',
#                                 search_page_ids = hr.id.64,
#                                 ad_delivery_date_max = '2020-11-03',
#                                 ad_delivery_date_min = '2020-01-01',
#                                 limit = 5000,
#                                 fields = "ad_data")
#hr.response.64 <- adlib_get(params = hr.query.64, token = token)
#hr.tibble.64 <- as_tibble(hr.response.64, type = "ad")
# Another of our series of errors...
hr.id.64.less <- hr.id.64[-8]
hr.query.64.less <- adlib_build_query(ad_reached_countries = 'US', 
                                 ad_active_status = 'ALL',
                                 search_page_ids = hr.id.64.less,
                                 ad_delivery_date_max = '2020-11-03',
                                 ad_delivery_date_min = '2020-01-01',
                                 limit = 5000,
                                 fields = "ad_data")
hr.response.64.less <- adlib_get(params = hr.query.64.less, token = token)
hr.tibble.64.less <- as_tibble(hr.response.64.less, type = "ad")
# Tony Gonzales is the one missing
fb.house <- rbind(fb.house, hr.tibble.64.less)
# More than 5000...need another query.
hr.query.64.less.2 <- adlib_build_query(ad_reached_countries = 'US', 
                                      ad_active_status = 'ALL',
                                      search_page_ids = hr.id.64.less,
                                      ad_delivery_date_max = '2020-06-30',
                                      ad_delivery_date_min = '2020-01-01',
                                      limit = 5000,
                                      fields = "ad_data")
hr.response.64.less.2 <- adlib_get(params = hr.query.64.less.2, token = token)
hr.tibble.64.less.2 <- as_tibble(hr.response.64.less.2, type = "ad")
fb.house <- rbind(fb.house, hr.tibble.64.less.2)

# Group 65 =====================================================================

hr.id.65 <- as.vector(houseframes[["65"]]$id)
#hr.query.65 <- adlib_build_query(ad_reached_countries = 'US', 
#                                 ad_active_status = 'ALL',
#                                 search_page_ids = hr.id.65,
#                                 ad_delivery_date_max = '2020-11-03',
#                                 ad_delivery_date_min = '2020-01-01',
#                                 limit = 5000,
#                                 fields = "ad_data")
#hr.response.65 <- adlib_get(params = hr.query.65, token = token)
#hr.tibble.65 <- as_tibble(hr.response.65, type = "ad")
# Our same error is back...
# Jaimy Z. Blanco
hr.id.65.less <- hr.id.65[-9]
hr.query.65 <- adlib_build_query(ad_reached_countries = 'US', 
                                 ad_active_status = 'ALL',
                                 search_page_ids = hr.id.65.less,
                                 ad_delivery_date_max = '2020-11-03',
                                 ad_delivery_date_min = '2020-01-01',
                                 limit = 5000,
                                 fields = "ad_data")
hr.response.65 <- adlib_get(params = hr.query.65, token = token)
hr.tibble.65 <- as_tibble(hr.response.65, type = "ad")

fb.house <- rbind(fb.house, hr.tibble.65)

# Group 66 =====================================================================

hr.id.66 <- as.vector(houseframes[["66"]]$id)
# Yet another of our error...
# Genevieve Collins
hr.id.66.less <- hr.id.66[-4]
hr.query.66 <- adlib_build_query(ad_reached_countries = 'US', 
                                 ad_active_status = 'ALL',
                                 search_page_ids = hr.id.66.less,
                                 ad_delivery_date_max = '2020-11-03',
                                 ad_delivery_date_min = '2020-01-01',
                                 limit = 5000,
                                 fields = "ad_data")
hr.response.66 <- adlib_get(params = hr.query.66, token = token)
hr.tibble.66 <- as_tibble(hr.response.66, type = "ad")
fb.house <- rbind(fb.house, hr.tibble.66)

# Group 67 =====================================================================

hr.id.67 <- as.vector(houseframes[["67"]]$id)
hr.query.67 <- adlib_build_query(ad_reached_countries = 'US', 
                                 ad_active_status = 'ALL',
                                 search_page_ids = hr.id.67,
                                 ad_delivery_date_max = '2020-11-03',
                                 ad_delivery_date_min = '2020-01-01',
                                 limit = 5000,
                                 fields = "ad_data")
hr.response.67 <- adlib_get(params = hr.query.67, token = token)
hr.tibble.67 <- as_tibble(hr.response.67, type = "ad")
fb.house <- rbind(fb.house, hr.tibble.67)


# Group 68 =====================================================================

hr.id.68 <- as.vector(houseframes[["68"]]$id)
hr.query.68 <- adlib_build_query(ad_reached_countries = 'US', 
                                 ad_active_status = 'ALL',
                                 search_page_ids = hr.id.68,
                                 ad_delivery_date_max = '2020-11-03',
                                 ad_delivery_date_min = '2020-01-01',
                                 limit = 5000,
                                 fields = "ad_data")
hr.response.68 <- adlib_get(params = hr.query.68, token = token)
hr.tibble.68 <- as_tibble(hr.response.68, type = "ad")
fb.house <- rbind(fb.house, hr.tibble.68)
# Over 5,000...need another one
hr.query.68.2 <- adlib_build_query(ad_reached_countries = 'US', 
                                 ad_active_status = 'ALL',
                                 search_page_ids = hr.id.68,
                                 ad_delivery_date_max = '2020-03-01',
                                 ad_delivery_date_min = '2020-01-01',
                                 limit = 5000,
                                 fields = "ad_data")
hr.response.68.2 <- adlib_get(params = hr.query.68.2, token = token)
hr.tibble.68.2 <- as_tibble(hr.response.68.2, type = "ad")
fb.house <- rbind(fb.house, hr.tibble.68.2)

# Group 69 =====================================================================

hr.id.69 <- as.vector(houseframes[["69"]]$id)
# Yet another of our error...two this time
# Nicholas Betts, Donald Beyer
hr.id.69.less <- hr.id.69[-c(5, 9)]
hr.query.69 <- adlib_build_query(ad_reached_countries = 'US', 
                                 ad_active_status = 'ALL',
                                 search_page_ids = hr.id.69.less,
                                 ad_delivery_date_max = '2020-11-03',
                                 ad_delivery_date_min = '2020-01-01',
                                 limit = 5000,
                                 fields = "ad_data")
hr.response.69 <- adlib_get(params = hr.query.69, token = token)
hr.tibble.69 <- as_tibble(hr.response.69, type = "ad")
fb.house <- rbind(fb.house, hr.tibble.69)

# Group 70 =====================================================================

hr.id.70 <- as.vector(houseframes[["70"]]$id)
hr.query.70 <- adlib_build_query(ad_reached_countries = 'US', 
                                 ad_active_status = 'ALL',
                                 search_page_ids = hr.id.70,
                                 ad_delivery_date_max = '2020-11-03',
                                 ad_delivery_date_min = '2020-01-01',
                                 limit = 5000,
                                 fields = "ad_data")
hr.response.70 <- adlib_get(params = hr.query.70, token = token)
hr.tibble.70 <- as_tibble(hr.response.70, type = "ad")
fb.house <- rbind(fb.house, hr.tibble.70)

# Group 71 =====================================================================

hr.id.71 <- as.vector(houseframes[["71"]]$id)
hr.query.71 <- adlib_build_query(ad_reached_countries = 'US', 
                                 ad_active_status = 'ALL',
                                 search_page_ids = hr.id.71,
                                 ad_delivery_date_max = '2020-11-03',
                                 ad_delivery_date_min = '2020-01-01',
                                 limit = 5000,
                                 fields = "ad_data")
hr.response.71 <- adlib_get(params = hr.query.71, token = token)
hr.tibble.71 <- as_tibble(hr.response.71, type = "ad")
fb.house <- rbind(fb.house, hr.tibble.71)

# Group 72 =====================================================================

hr.id.72 <- as.vector(houseframes[["72"]]$id)
hr.query.72 <- adlib_build_query(ad_reached_countries = 'US', 
                                 ad_active_status = 'ALL',
                                 search_page_ids = hr.id.72,
                                 ad_delivery_date_max = '2020-11-03',
                                 ad_delivery_date_min = '2020-01-01',
                                 limit = 5000,
                                 fields = "ad_data")
hr.response.72 <- adlib_get(params = hr.query.72, token = token)
hr.tibble.72 <- as_tibble(hr.response.72, type = "ad")
fb.house <- rbind(fb.house, hr.tibble.72)

# Group 73 =====================================================================

hr.id.73 <- as.vector(houseframes[["73"]]$id)
hr.query.73 <- adlib_build_query(ad_reached_countries = 'US', 
                                 ad_active_status = 'ALL',
                                 search_page_ids = hr.id.73,
                                 ad_delivery_date_max = '2020-11-03',
                                 ad_delivery_date_min = '2020-01-01',
                                 limit = 5000,
                                 fields = "ad_data")
hr.response.73 <- adlib_get(params = hr.query.73, token = token)
hr.tibble.73 <- as_tibble(hr.response.73, type = "ad")
fb.house <- rbind(fb.house, hr.tibble.73)

# Group 74 =====================================================================

hr.id.74 <- as.vector(houseframes[["74"]]$id)
hr.query.74 <- adlib_build_query(ad_reached_countries = 'US', 
                                 ad_active_status = 'ALL',
                                 search_page_ids = hr.id.74,
                                 ad_delivery_date_max = '2020-11-03',
                                 ad_delivery_date_min = '2020-01-01',
                                 limit = 5000,
                                 fields = "ad_data")
hr.response.74 <- adlib_get(params = hr.query.74, token = token)
hr.tibble.74 <- as_tibble(hr.response.74, type = "ad")
fb.house <- rbind(fb.house, hr.tibble.74)

# Tests with our outliers ======================================================

## Yvette Herrell, NM, 1750700000000000
## Ted Budd, NC, 1585290000000000
## Tony Gonzales, TX, 2146200000000000
## Jaimy Z. Blanco, TX, 1773150000000000
## Genevieve Collins, TX, 2338980000000000
## Nicholas Betts, VA, 1911030000000000
## Donald Beyer, VA, 1411980000000000

# Seems to be the case that these page IDs got corrupted - I'll go searching
# for them.

herrell <- 1750699881894920
herrell.query <- adlib_build_query(ad_reached_countries = 'US', 
                                   ad_active_status = 'ALL',
                                   search_page_ids = as.vector(herrell),
                                   ad_delivery_date_max = '2020-11-03',
                                   ad_delivery_date_min = '2020-01-01',
                                   limit = 10000,
                                   fields = "ad_data")
herrell.response <- adlib_get(params = herrell.query, token = token)
herrell.tibble <- as_tibble(herrell.response, type = "ad")

herrell <- 1750699881894920
budd <- 1585291608451100
gonzales <- 2146201085498440
blanco <- 1773152856029750
collins <- 2338979479650120
betts <- 1911027705653860
beyer <- 1411983362382190

id_vector <- as.vector(c(herrell, budd, gonzales, blanco, collins, betts, 
                         beyer))

query <- adlib_build_query(ad_reached_countries = 'US', 
                           ad_active_status = 'ALL',
                           search_page_ids = id_vector,
                           ad_delivery_date_max = '2020-11-03',
                           ad_delivery_date_min = '2020-01-01',
                           limit = 5000,
                           fields = "ad_data")

response <- adlib_get(params = query, token = token)
results.tibble <- as_tibble(response, type = "ad")

fb.house <- rbind(fb.house, results.tibble)

# Removing Duplicates, and Saving Data =========================================

fb.house.distinct <- distinct(fb.house)

save(fb.house.distinct, file = "data/raw/fb/fb-house-2020-ads.rda")

# For Loop Experimentation =====================================================

loopdat <- housedat.01
loopdatvec <- as.vector(loopdat$id)
for (i in loopdatvec){
  test_query_i <- adlib_build_query(ad_reached_countries = 'US', 
                                   ad_active_status = 'ALL',
                                   search_page_ids = as.vector(i),
                                   ad_delivery_date_max = '2020-11-03',
                                   ad_delivery_date_min = '2020-01-01',
                                   limit = 5000,
                                   fields = "ad_data")
  test_response_i <- adlib_get(params = test_query_i, token = token)
  test_tibble_i <- as_tibble(test_response_i, type = "ad")
}
## I can't figure out how to craft a for loop that'll work for Radlibrary.
## It only pulls two ads from one candidate.

loopdf <- as.data.frame(loopdatvec)
for (i in loopdf){
  test_query_i <- adlib_build_query(ad_reached_countries = 'US', 
                                    ad_active_status = 'ALL',
                                    search_page_ids = as.vector(i),
                                    ad_delivery_date_max = '2020-11-03',
                                    ad_delivery_date_min = '2020-01-01',
                                    limit = 5000,
                                    fields = "ad_data")
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
for (i in testwith12df){
  test_query_i <- adlib_build_query(ad_reached_countries = 'US', 
                                    ad_active_status = 'ALL',
                                    search_page_ids = as.vector(i),
                                    ad_delivery_date_max = '2020-11-03',
                                    ad_delivery_date_min = '2020-01-01',
                                    limit = 5000,
                                    fields = "ad_data")
  test_response_i <- adlib_get(params = test_query_i, token = token)
  test_tibble_i <- as_tibble(test_response_i, type = "ad")
}
# It doesn't work. "Error in adlib_build_query(ad_reached_countries = "US", 
# ad_active_status = "ALL",  : Can only search 10 page IDs at a time."


# The loop needs to run multiple queries and return multiple data frames for
# it to be useful. Otherwise, it's counterintuitive.





