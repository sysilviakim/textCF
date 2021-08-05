# Facebook Ad Library sample code - Ossoff and Perdue ==========================

# Basics =======================================================================

## Download and load any necessary packages
#install.packages("devtools")
#devtools::install_github("facebookresearch/Radlibrary")
# library(Radlibrary)

# Setup ========================================================================

# Copy the token directly from the Facebook Ad Library (will be under the
# 'Graph API Explorer' tool). Paste it into the console when prompted. Token 
# will expire within an hour of generation, so be mindful of that.
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
