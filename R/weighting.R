source(here::here("R", "utilities.R"))

# Load data ====================================================================
load(here("data", "tidy", "merged_unique.Rda"))
## Because of existing format, splitting df_unique into list
df_unique <- split(df_unique, f = df_unique$chamber) # Split data
names(df_unique) = c("house","senate")

load(here("data", "tidy", "fb_house_merged.Rda"))
load(here("data", "tidy", "fb_senate_merged.Rda"))

# Merging weighting variables into fb_house ====================================

# Key variables we need:
## Date: ad_creation_time
## Impressions: impressions_lower, impressions_upper
## Money: spend_lower, spend_upper
# Need to merge... will use fb_ad_library_id, ad_creative_body -- different
## capitalization of candidate variables will cause headaches

# Sanity checks, before we get started...
assert_that(!any(is.na(fb_house$ad_creative_body))) # True
# assert_that(!any(is.na(df_unique$house$ad_creative_body))) # NOT True
## As this is about ad texts, we will drop rows where creative body is NA
df_unique$house <- df_unique$house[!is.na(
  df_unique$house$ad_creative_body
), ]
assert_that(!any(is.na(df_unique$house$ad_creative_body))) # True

house_weights <- merge(
  x = fb_house,
  y = df_unique$house[, c(
    "fb_ad_library_id", "ad_creative_body",
    "ad_creation_time",
    "impressions_lower", "impressions_upper",
    "spend_lower", "spend_upper"
  )],
  by = c("fb_ad_library_id", "ad_creative_body"), all.x = TRUE
)
# Sanity checks
assert_that(!any(is.na(house_weights$ad_creative_body))) # True
assert_that(!any(is.na(house_weights$TOXICITY))) # True
assert_that(!any(is.na(house_weights$ad_creation_time))) # True

# Are there any duplicated rows?
assert_that(!any(duplicated(house_weights)))
# Will include code to remove them -- however, for the actual run, will double-
# check about this step
house_weights <- house_weights[!duplicated(house_weights), ]
# Number of rows goes significantly down with this...before, though, it was
# greater than the number of rows in fb_df_unique$house, which didn't make sense

# Adding toxic classifier -- originally 0.2, changed to 0.5
## What was 
house_weights <- house_weights %>%
  mutate(toxic = case_when(
    toxicity >= 0.5 ~ "toxic",
    toxicity < 0.5 ~ "nontoxic"
  ))
sum(house_weights$toxic == "toxic") # 1984/144464...1.373%
assert_that(!any(is.na(house_weights$toxic))) # True
assert_that(!any(is.na(house_weights$spend_lower))) # True
assert_that(!any(is.na(house_weights$spend_upper))) # True
assert_that(!any(is.na(house_weights$impressions_upper))) # NOT true...
assert_that(!any(is.na(house_weights$impressions_lower))) # True

# Merging weighting variables into fb_senate ===================================

# Sanity checks, before we get started...
assert_that(!any(is.na(fb_senate$ad_creative_body)))
# assert_that(!any(is.na(df_unique$senate$ad_creative_body)))
## Not True...since this is about the ad texts, we'll remove ones without the
## ad creative body
df_unique$senate <- df_unique$senate[!is.na(
  df_unique$senate$ad_creative_body
), ]
assert_that(!any(is.na(df_unique$senate$ad_creative_body))) # True now
senate_weights <- merge(
  x = fb_senate,
  y = df_unique$senate[, c(
    "fb_ad_library_id", "ad_creative_body",
    "ad_creation_time",
    "impressions_lower", "impressions_upper",
    "spend_lower", "spend_upper", "party"
  )],
  by = c("fb_ad_library_id", "ad_creative_body"), all.x = TRUE
)
# The party variable is NA for all those who were not running in 2020...this is
# something to turn to our advantage! We can remove all those rows before we
# plot impressions and funds raised

# Sanity checks
assert_that(!any(is.na(senate_weights$ad_creative_body))) # True
assert_that(!any(is.na(senate_weights$toxicity))) # True
assert_that(!any(is.na(senate_weights$ad_creation_time))) # True
# Are there any duplicated rows?
assert_that(!any(duplicated(senate_weights)))
# Same deal as with house
senate_weights <- senate_weights[!duplicated(senate_weights), ]
assert_that(!any(duplicated(senate_weights)))

# Adding toxic classifier -- originally 0.2, changed to 0.5
senate_weights <- senate_weights %>%
  mutate(toxic = case_when(
    toxicity >= 0.5 ~ "toxic",
    toxicity < 0.5 ~ "nontoxic"
  ))
sum(senate_weights$toxic == "toxic") # 1617/147929...1.093%
assert_that(!any(is.na(senate_weights$toxic))) # True
assert_that(!any(is.na(senate_weights$spend_lower))) # True
assert_that(!any(is.na(senate_weights$spend_upper))) # True
assert_that(!any(is.na(senate_weights$impressions_upper))) # NOT true...
assert_that(!any(is.na(senate_weights$impressions_lower))) # True

# Separating out data for relevant plots =======================================

# Toxic House, Toxic Senate, Nontoxic House, Nontoxic Senate will be the basis
# for the time being -- can rework later

## House Toxic
house_tox <- subset(house_weights, toxic == "toxic")
house_tox$ad_creation_time <- format(house_tox$ad_creation_time,
  format = "%Y-%m"
)

## House Non-Toxic
house_nontox <- subset(house_weights, toxic == "nontoxic")
house_nontox$ad_creation_time <- format(house_nontox$ad_creation_time,
  format = "%Y-%m"
)
assert_that(nrow(house_tox) + nrow(house_nontox) == nrow(house_weights)) # True

# For the Senate, for these plots, remove Senate candidates who weren't up for
# re-election
senate_weights <- senate_weights[!is.na(senate_weights$party), ]
# Down to 92,746
## NOTE: 1090/92746: 1.175%
## Since we're focusing on folks who were candidates here, this is the one
## to use

# Senate Toxic
senate_tox <- subset(senate_weights, toxic == "toxic")
senate_tox$ad_creation_time <- format(senate_tox$ad_creation_time,
  format = "%Y-%m"
)

# Senate Non-Toxic
senate_nontox <- subset(senate_weights, toxic == "nontoxic")
senate_nontox$ad_creation_time <- format(
  senate_nontox$ad_creation_time,
  format = "%Y-%m"
)
assert_that(nrow(senate_tox) + nrow(senate_nontox) == nrow(senate_weights))

# Weighting 1: Toxicity and funds spent ========================================

## We want to group by month...will also take out any ads that initially were
## aired in 2018

# HR Toxic:
hr_tox_funds <- house_tox %>%
  group_by(ad_creation_time) %>%
  summarize(spend_upper)
hr_tox_funds_plotting <- aggregate(
  spend_upper ~ ad_creation_time,
  hr_tox_funds, sum
)
# None in 2018 -- none in 2020-12, either -- to make things neat, will add in
# a 0 row
hr_tox_funds_plotting[nrow(hr_tox_funds_plotting) + 1, ] <- c("2020-12", 0)
hr_tox_funds_plotting$spend_upper <-
  as.numeric(hr_tox_funds_plotting$spend_upper)

# HR Nontoxic
hr_nontox_funds <- house_nontox %>%
  group_by(ad_creation_time) %>%
  summarize(spend_upper)
hr_nontox_funds_plotting <- aggregate(
  spend_upper ~ ad_creation_time,
  hr_nontox_funds, sum
)
hr_nontox_funds_plotting <- hr_nontox_funds_plotting[!grepl(
  "2018",
  hr_nontox_funds_plotting$ad_creation_time
), ]

# Senate Toxic:
senate_tox_funds <- senate_tox %>%
  group_by(ad_creation_time) %>%
  summarize(spend_upper)
senate_tox_funds_plotting <- aggregate(
  spend_upper ~ ad_creation_time,
  senate_tox_funds, sum
)
senate_tox_funds_plotting <- senate_tox_funds_plotting[!grepl(
  "2018",
  senate_tox_funds_plotting$ad_creation_time
), ]

# senate Nontoxic
senate_nontox_funds <- senate_nontox %>%
  group_by(ad_creation_time) %>%
  summarize(spend_upper)
senate_nontox_funds_plotting <- aggregate(
  spend_upper ~ ad_creation_time,
  senate_nontox_funds, sum
)
senate_nontox_funds_plotting <- senate_nontox_funds_plotting[!grepl(
  "2018",
  senate_nontox_funds_plotting$ad_creation_time
), ]

# HR Toxic
## Date format was lost -- will have to re-convert from character
library(scales)
house_tox_funds_ts <- ggplot(hr_tox_funds_plotting, aes(
  x = ad_creation_time,
  y = spend_upper
)) +
  theme_bw() +
 coord_cartesian(ylim = c(0, 20000000)) +
  geom_bar(stat="identity") +
  labs(x = "Initial Distribution Date", 
       y = "Total Funds Spent (Upper Estimate)") +
  ggtitle("House Toxic Advertisements") +
  theme(
    axis.text.x = element_text(angle = 30, hjust = 1),
    plot.title = element_text(hjust = 0.5)
  ) +
  scale_x_discrete(breaks = c(
    "2019-02", "2019-05", "2019-08", "2019-11",
    "2020-02", "2020-05", "2020-08", "2020-11"
  )) +
  scale_y_continuous(labels = unit_format(unit = "M", scale = 1e-6)) # +
house_tox_funds_ts

# HR Nontoxic
house_nontox_funds_ts <- ggplot(
  hr_nontox_funds_plotting,
  aes(
    x = ad_creation_time,
    y = spend_upper
  )
) +
  theme_bw() +
  coord_cartesian(ylim = c(0, 20000000)) +
  geom_bar(stat="identity") +
  labs(x = "Initial Distribution Date", y = "Total Funds Spent (Upper Estimate)") +
  ggtitle("House Nontoxic Advertisements") +
  theme(
    axis.text.x = element_text(angle = 30, hjust = 1),
    plot.title = element_text(hjust = 0.5)
  ) +
  scale_x_discrete(breaks = c(
    "2019-02", "2019-05", "2019-08", "2019-11",
    "2020-02", "2020-05", "2020-08", "2020-11"
  )) +
  scale_y_continuous(labels = unit_format(unit = "M", scale = 1e-6)) # +
house_nontox_funds_ts

# Senate Toxic
senate_tox_funds_ts <- ggplot(
  senate_tox_funds_plotting,
  aes(
    x = ad_creation_time,
    y = spend_upper
  )
) +
  theme_bw() +
  coord_cartesian(ylim = c(0, 20000000)) +
  geom_bar(stat="identity") +
  labs(x = "Initial Distribution Date", 
       y = "Total Funds Spent (Upper Estimate)") +
  ggtitle("Senate Toxic Advertisements") +
  theme(
    axis.text.x = element_text(angle = 30, hjust = 1),
    plot.title = element_text(hjust = 0.5)
  ) +
  scale_x_discrete(breaks = c(
    "2019-02", "2019-05", "2019-08", "2019-11",
    "2020-02", "2020-05", "2020-08", "2020-11"
  )) +
  scale_y_continuous(labels = unit_format(unit = "M", scale = 1e-6)) # +
senate_tox_funds_ts

# Senate Nontoxic
senate_nontox_funds_ts <- ggplot(
  senate_nontox_funds_plotting,
  aes(
    x = ad_creation_time,
    y = spend_upper
  )
) +
  theme_bw() +
  coord_cartesian(ylim = c(0, 20000000)) +
  geom_bar(stat="identity") +
  labs(x = "Initial Distribution Date", 
       y = "Total Funds Spent (Upper Estimate)") +
  ggtitle("Senate Nontoxic Advertisements") +
  theme(
    axis.text.x = element_text(angle = 30, hjust = 1),
    plot.title = element_text(hjust = 0.5)
  ) +
  scale_x_discrete(breaks = c(
    "2019-02", "2019-05", "2019-08", "2019-11",
    "2020-02", "2020-05", "2020-08", "2020-11"
  )) +
  scale_y_continuous(labels = unit_format(unit = "M", scale = 1e-6)) # +
senate_nontox_funds_ts
# Arranging figure
funds_toxicity <- ggarrange(house_tox_funds_ts,
  house_nontox_funds_ts,
  senate_tox_funds_ts,
  senate_nontox_funds_ts,
  ncol = 2, nrow = 2
)
funds_toxicity <- annotate_figure(
  funds_toxicity,
  top = text_grob("Total Funds Spent on Ads",
    color = "black", face = "bold",
    size = 14
  )
)
funds_toxicity

ggsave("fig/funds_toxicity_ts.pdf",
  plot = funds_toxicity,
  width = 10, height = 8
)

# Weighting 2: Toxicity and impressions ========================================

## Because of the fact that there are some NAs in the impressions_upper column
## but no NAs in the impressions_lower, we'll use impressions_lower.
## The maximum value in impressions_upper is 999999...I take this to mean that
## the NAs there are ads with predicted upper impressions >= 1 million

# HR Toxic:
hr_tox_imps <- house_tox %>%
  group_by(ad_creation_time) %>%
  summarize(impressions_lower)
hr_tox_imps_plotting <- aggregate(
  impressions_lower ~ ad_creation_time,
  hr_tox_imps, sum
)
hr_tox_imps_plotting[nrow(hr_tox_imps_plotting) + 1, ] <- c("2020-12", 0)
hr_tox_imps_plotting$impressions_lower <-
  as.numeric(hr_tox_imps_plotting$impressions_lower)

# HR Nontoxic
hr_nontox_imps <- house_nontox %>%
  group_by(ad_creation_time) %>%
  summarize(impressions_lower)
hr_nontox_imps_plotting <- aggregate(
  impressions_lower ~ ad_creation_time,
  hr_nontox_imps, sum
)
hr_nontox_imps_plotting <- hr_nontox_imps_plotting[!grepl(
  "2018",
  hr_nontox_imps_plotting$ad_creation_time
), ]

# Senate Toxic:
senate_tox_imps <- senate_tox %>%
  group_by(ad_creation_time) %>%
  summarize(impressions_lower)
senate_tox_imps_plotting <- aggregate(
  impressions_lower ~ ad_creation_time,
  senate_tox_imps, sum
)
senate_tox_imps_plotting <- senate_tox_imps_plotting[!grepl(
  "2018",
  senate_tox_imps_plotting$ad_creation_time
), ]

# senate Nontoxic
senate_nontox_imps <- senate_nontox %>%
  group_by(ad_creation_time) %>%
  summarize(impressions_lower)
senate_nontox_imps_plotting <- aggregate(
  impressions_lower ~ ad_creation_time,
  senate_nontox_imps, sum
)
senate_nontox_imps_plotting <- senate_nontox_imps_plotting[!grepl(
  "2018",
  senate_nontox_imps_plotting$ad_creation_time
), ]

# HR Toxic
house_tox_imps_ts <- ggplot(
  hr_tox_imps_plotting,
  aes(
    x = ad_creation_time,
    y = impressions_lower
  )
) +
  theme_bw() +
  coord_cartesian(ylim = c(0, 600000000)) +
  geom_bar(stat="identity") +
  labs(x = "Initial Distribution Date", 
       y = "Total Impressions (Lower Estimate)") +
  ggtitle("House Toxic Advertisements") +
  theme(
    axis.text.x = element_text(angle = 30, hjust = 1),
    plot.title = element_text(hjust = 0.5)
  ) +
  scale_x_discrete(breaks = c(
    "2019-02", "2019-05", "2019-08", "2019-11",
    "2020-02", "2020-05", "2020-08", "2020-11"
  )) +
  scale_y_continuous(labels = unit_format(unit = "M", scale = 1e-6)) # +
house_tox_imps_ts
# HR Nontoxic
house_nontox_imps_ts <- ggplot(
  hr_nontox_imps_plotting,
  aes(
    x = ad_creation_time,
    y = impressions_lower
  )
) +
  theme_bw() +
  geom_bar(stat="identity") +
  coord_cartesian(ylim = c(0, 600000000)) +
  labs(x = "Initial Distribution Date", 
       y = "Total Impressions (Lower Estimate)") +
  ggtitle("House Nontoxic Advertisements") +
  theme(
    axis.text.x = element_text(angle = 30, hjust = 1),
    plot.title = element_text(hjust = 0.5)
  ) +
  scale_x_discrete(breaks = c(
    "2019-02", "2019-05", "2019-08", "2019-11",
    "2020-02", "2020-05", "2020-08", "2020-11"
  )) +
  scale_y_continuous(labels = unit_format(unit = "M", scale = 1e-6)) # +
house_nontox_imps_ts
# Senate Toxic
senate_tox_imps_ts <- ggplot(
  senate_tox_imps_plotting,
  aes(
    x = ad_creation_time,
    y = impressions_lower
  )
) +
  theme_bw() +
  coord_cartesian(ylim = c(0, 600000000)) +
  geom_bar(stat="identity") +
  labs(x = "Initial Distribution Date", 
       y = "Total Impressions (Lower Estimate)") +
  ggtitle("Senate Toxic Advertisements") +
  theme(
    axis.text.x = element_text(angle = 30, hjust = 1),
    plot.title = element_text(hjust = 0.5)
  ) +
  scale_x_discrete(breaks = c(
    "2019-02", "2019-05", "2019-08", "2019-11",
    "2020-02", "2020-05", "2020-08", "2020-11"
  )) +
  scale_y_continuous(labels = unit_format(unit = "M", scale = 1e-6)) # +
senate_tox_imps_ts
# Senate Nontoxic
senate_nontox_imps_ts <- ggplot(
  senate_nontox_imps_plotting,
  aes(
    x = ad_creation_time,
    y = impressions_lower
  )
) +
  theme_bw() +
  coord_cartesian(ylim = c(0, 600000000)) +
  geom_bar(stat="identity") +
  labs(x = "Initial Distribution Date", 
       y = "Total Impressions (Lower Estimate)") +
  ggtitle("Senate Nontoxic Advertisements") +
  theme(
    axis.text.x = element_text(angle = 30, hjust = 1),
    plot.title = element_text(hjust = 0.5)
  ) +
  scale_x_discrete(breaks = c(
    "2019-02", "2019-05", "2019-08", "2019-11",
    "2020-02", "2020-05", "2020-08", "2020-11"
  )) +
  scale_y_continuous(labels = unit_format(unit = "M", scale = 1e-6)) # +
senate_nontox_imps_ts

# Arranging figure
imps_toxicity <- ggarrange(house_tox_imps_ts,
  house_nontox_imps_ts,
  senate_tox_imps_ts,
  senate_nontox_imps_ts,
  ncol = 2, nrow = 2
)
imps_toxicity <- annotate_figure(
  imps_toxicity,
  top = text_grob("Total Impressions of Ads",
    color = "black", face = "bold",
    size = 14
  )
)
imps_toxicity

ggsave("fig/imps_toxicity_ts.pdf",
  plot = imps_toxicity,
  width = 10, height = 8
)

# Illustrative Percentages =====================================================

# House
## Percent of Ads that are Toxic
nrow(house_tox) / (nrow(house_tox) + nrow(house_nontox))
### 1.373% of House ads are toxic
## Percent of Funds that go to Toxic ads
sum(hr_tox_funds$spend_upper) / (sum(hr_tox_funds$spend_upper) + 
                                   sum(hr_nontox_funds$spend_upper))
### 2.474% of funding goes to toxic ads
## Percent of Impressions that go to Toxic Ads
sum(hr_tox_imps$impressions_lower) / (sum(hr_tox_imps$impressions_lower) + 
                                   sum(hr_nontox_imps$impressions_lower))
### 2.202% of impressions go to Toxic ads

# Senate
## Percent of Ads that are Toxic
nrow(senate_tox) / (nrow(senate_tox) + nrow(senate_nontox))
### 1.175% of Senate ads (for senators running in 2020) are toxic
## Percent of Funds that go to Toxic ads
sum(senate_tox_funds$spend_upper) / (sum(senate_tox_funds$spend_upper) + 
                                   sum(senate_nontox_funds$spend_upper))
### 0.799% of funding goes to toxic ads...other direction from HR
  ## Percent of Impressions that go to Toxic Ads
sum(senate_tox_imps$impressions_lower) / (
  sum(senate_tox_imps$impressions_lower) +
    sum(senate_nontox_imps$impressions_lower))
### 0.909% of impressions go to Toxic ads

# Trump Mentions and Toxicity ==================================================
### Split into House/Senate, GOP/Dem, mention/non-mention
house_gop_trump <- df_unique$house[df_unique$house$word_trump == 1 &
                                   df_unique$house$party == "Republican", ]
house_gop_notrump <- df_unique$house[df_unique$house$word_trump == 0 &
                                     df_unique$house$party == "Republican", ]
house_dem_trump <- df_unique$house[df_unique$house$word_trump == 1 &
                                     df_unique$house$party == "Democrat", ]
house_dem_notrump <- df_unique$house[df_unique$house$word_trump == 0 &
                                       df_unique$house$party == "Democrat", ]
senate_gop_trump <- df_unique$senate[df_unique$senate$word_trump == 1 &
                                     df_unique$senate$party == "Republican", ]
senate_gop_notrump <- df_unique$senate[df_unique$senate$word_trump == 0 &
                                       df_unique$senate$party == "Republican", ]
senate_dem_trump <- df_unique$senate[df_unique$senate$word_trump == 1 &
                                     df_unique$senate$party == "Democrat", ]
senate_dem_notrump <- df_unique$senate[df_unique$senate$word_trump == 0 &
                                       df_unique$senate$party == "Democrat", ]

### Difference-in-Means Tests
# House GOP
t.test(house_gop_trump$toxicity, house_gop_notrump$toxicity)
# House Dems
t.test(house_dem_trump$toxicity, house_dem_notrump$toxicity)
# Senate GOP
t.test(senate_gop_trump$toxicity, senate_gop_notrump$toxicity)
# Senate Dems
t.test(senate_dem_trump$toxicity, senate_dem_notrump$toxicity)

# Loeffler and Warnock Means ===================================================
loeffler <- df_unique$senate[df_unique$senate$candidate ==
                                    "kelly loeffler", ]
warnock <- df_unique$senate[df_unique$senate$candidate ==
                                   "raphael warnock", ]
loeffler_donors <- loeffler[loeffler$financial == "Donor-targeting", ]
loeffler_voters <- loeffler[loeffler$financial == "Voter-targeting", ]
warnock_donors <- warnock[warnock$financial == "Donor-targeting", ]
warnock_voters <- warnock[warnock$financial == "Voter-targeting", ]

mean(loeffler_donors$toxicity) # 0.06855473
mean(loeffler_voters$toxicity) # 0.06029068
mean(warnock_donors$toxicity) # 0.222693
mean(warnock_voters$toxicity) # 0.1004144
