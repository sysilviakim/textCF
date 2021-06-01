source(here::here("R", "utilities.R"))

# Load ActBlue JS scrape results ===============================================
load(here("data/raw/2022/winred/winred_text_scraped.Rda"))

## If office is simply a two-character state abbr., state leg.
## Exclude parties
winred_federal <- winred_text %>%
  filter(grepl("-", office)) %>%
  mutate(office = gsub("-AL$", "-0", office))

save(winred_federal, file = here("data/tidy/winred_federal_2022.Rda"))
