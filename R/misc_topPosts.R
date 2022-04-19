library(tidyverse)
# source(here::here("R", "utilities.R"))

# Load data ====================================================================
fb_house <- readRDS(here::here("data", "tidy", "fb_perspective_House.RDS"))
fb_senate <- readRDS(here::here("data", "tidy", "fb_perspective_Senate.RDS"))

# Summary dataset ======================================================
df <- bind_rows(
  fb_house %>% mutate(chamber = "House"),
  fb_senate %>% mutate(chamber = "Senate")
) 



# Use a .2 toxicity cut-off to classify an ad as containing toxic language
# Check patterns by ad type:
df %>% 
  mutate(toxic = ifelse(TOXICITY>=.2,1,0)) %>%
  count(toxic,financial) %>%
  group_by(financial) %>%
  mutate(prop = n / sum(n))

# Use a .2 toxicity cut-off to classify an ad as containing toxic language
# Check patterns by party:
df %>% 
  mutate(toxic = ifelse(TOXICITY>=.2,1,0)) %>%
  count(toxic,party) %>%
  group_by(party) %>%
  mutate(prop = n / sum(n))

# Data coverage:
d %>% 
  count(chamber,party)

# Top 100 most toxic ads by party
df %>% 
  arrange(-TOXICITY) %>%
  slice(1:100) %>%
  count(party)

# See the text:
df %>% 
  arrange(-TOXICITY) %>%
  slice(1:10) %>%
  pull(ad_creative_body)


df %>% 
  group_by(TOXICITY >=.2) %>%
  summarise(
  m = mean(TOXICITY),
  N = n()
)

