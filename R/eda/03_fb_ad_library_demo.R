# Valid fields, per documentation:
# ad_data - Choose this to include all columns for an ad_table table
# demographic_data - Choose this to include all columns for demographic_table
# region_data - Choose this to include all columns for a region_table
# ad_creation_time
# ad_creative_body
# ad_creative_link_caption
# ad_creative_link_description
# ad_creative_link_title
# ad_delivery_start_time
# ad_delivery_stop_time
# ad_snapshot_url
# currency
# demographic_distribution
# funding_entity
# impressions
# page_id
# page_name
# potential_reach
# publisher_platforms
# region_distribution
# spend
token <- readline()
source(here::here("R", "utilities.R"))

query <- Radlibrary::adlib_build_query(
  ad_reached_countries = "US",
  search_terms = "medicare",
  fields = c(
    "ad_snapshot_url", "ad_creation_time",
    "ad_creative_body", "ad_delivery_start_time",
    "ad_delivery_stop_time", "funding_entity",
    "impressions", "page_name",
    "potential_reach", "publisher_platforms",
    "spend", "demographic_distribution",
    "region_distribution"
  ),
  limit = 1000
)
response <- Radlibrary::adlib_get(query, token)

demographic_data <- parse_response(response)
save(demographic_data, file = here::here("data/tidy/fb_adlibrary_scrape_demo.Rda"))
