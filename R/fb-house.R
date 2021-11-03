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

fb.house <- rbind(fb.house, hr.tibble.42)
