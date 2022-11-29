source(here::here("R", "utilities.R"))

# Load data ====================================================================
fb_house <- read_rds("data/tidy/fb_perspective_House_20220426.RDS")
fb_senate <- read_rds("data/tidy/fb_perspective_Senate_20220426.RDS")
load(here("data", "tidy", "fb_matched.Rda"))
matched <- fb_matched

# Merging weighting variables into fb_house ====================================

# Key variables we need:
## Date: ad_creation_time
## Impressions: impressions_lower, impressions_upper
## Money: spend_lower, spend_upper
# Need to merge... will use fb_ad_library_id, ad_creative_body -- different
## capitalization of candidate variables will cause headaches

# Sanity checks, before we get started...
assert_that(!any(is.na(fb_house$ad_creative_body))) # True
#assert_that(!any(is.na(matched$house$ad_creative_body))) # NOT True
## As this is about ad texts, we will drop rows where creative body is NA
matched$house <- matched$house[!is.na(
  matched$house$ad_creative_body),]
assert_that(!any(is.na(matched$house$ad_creative_body))) # True

house_weights <- merge(
  x = fb_house,
  y = matched$house[, c("fb_ad_library_id", "ad_creative_body", 
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
# greater than the number of rows in fb_matched$house, which didn't make sense

# Adding toxic classifier -- originally 0.2, changed to 0.5
## What was 
house_weights <- house_weights %>%
  mutate(toxic = case_when(
    toxicity >= 0.5 ~ "toxic",
    toxicity < 0.5 ~ "nontoxic"
  ))
sum(house_weights$toxic == "toxic") # 1984/144464...1.373%
assert_that(!any(is.na(house_weights$toxic))) # True
assert_that(!any(is.na(house_weights$spend_lower)))# True
assert_that(!any(is.na(house_weights$spend_upper)))# True
assert_that(!any(is.na(house_weights$impressions_upper))) # NOT true...
assert_that(!any(is.na(house_weights$impressions_lower))) # True

# Merging weighting variables into fb_senate ===================================

# Sanity checks, before we get started...
assert_that(!any(is.na(fb_senate$ad_creative_body)))
# assert_that(!any(is.na(matched$senate$ad_creative_body)))
## Not True...since this is about the ad texts, we'll remove ones without the
## ad creative body
matched$senate <- matched$senate[!is.na(
  matched$senate$ad_creative_body),]
assert_that(!any(is.na(matched$senate$ad_creative_body))) # True now
senate_weights <- merge(
  x = fb_senate,
  y = matched$senate[, c("fb_ad_library_id", "ad_creative_body", 
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
assert_that(!any(is.na(senate_weights$spend_lower)))# True
assert_that(!any(is.na(senate_weights$spend_upper)))# True
assert_that(!any(is.na(senate_weights$impressions_upper))) # NOT true...
assert_that(!any(is.na(senate_weights$impressions_lower))) # True

# Separating out data for relevant plots =======================================

# Toxic House, Toxic Senate, Nontoxic House, Nontoxic Senate will be the basis
# for the time being -- can rework later

## House Toxic
house_tox <- subset(house_weights, toxic == "toxic")
house_tox$ad_creation_time <- format(house_tox$ad_creation_time,
       format = "%Y-%m")

## House Non-Toxic
house_nontox <- subset(house_weights, toxic == "nontoxic")
house_nontox$ad_creation_time <- format(house_nontox$ad_creation_time,
                                     format = "%Y-%m")
assert_that(nrow(house_tox)+nrow(house_nontox) == nrow(house_weights)) # True

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
                                        format = "%Y-%m")

# Senate Non-Toxic
senate_nontox <- subset(senate_weights, toxic == "nontoxic")
senate_nontox$ad_creation_time <- format(senate_nontox$ad_creation_time,
                                        format = "%Y-%m")
assert_that(nrow(senate_tox)+nrow(senate_nontox) == nrow(senate_weights)) # True

# Weighting 1: Toxicity and funds spent ========================================

## We want to group by month...will also take out any ads that initially were
## aired in 2018

# HR Toxic:
hr_tox_funds <- house_tox %>% 
  group_by(ad_creation_time) %>%
  summarize(spend_upper)
hr_tox_funds_plotting <- aggregate(spend_upper ~ ad_creation_time,
                          hr_tox_funds, sum)
# None in 2018 -- none in 2020-12, either -- to make things neat, will add in
# a 0 row
hr_tox_funds_plotting[nrow(hr_tox_funds_plotting) + 1,] <- c("2020-12", 0)
hr_tox_funds_plotting$spend_upper <- as.numeric(hr_tox_funds_plotting$spend_upper)

# HR Nontoxic
hr_nontox_funds <- house_nontox %>% 
  group_by(ad_creation_time) %>%
  summarize(spend_upper)
hr_nontox_funds_plotting <- aggregate(spend_upper ~ ad_creation_time,
                                   hr_nontox_funds, sum)
hr_nontox_funds_plotting <- hr_nontox_funds_plotting[!grepl("2018", 
                                                            hr_nontox_funds_plotting$ad_creation_time),]

# Senate Toxic:
senate_tox_funds <- senate_tox %>% 
  group_by(ad_creation_time) %>%
  summarize(spend_upper)
senate_tox_funds_plotting <- aggregate(spend_upper ~ ad_creation_time,
                                   senate_tox_funds, sum)
senate_tox_funds_plotting <- senate_tox_funds_plotting[!grepl("2018", 
                                                              senate_tox_funds_plotting$ad_creation_time),]

# senate Nontoxic
senate_nontox_funds <- senate_nontox %>% 
  group_by(ad_creation_time) %>%
  summarize(spend_upper)
senate_nontox_funds_plotting <- aggregate(spend_upper ~ ad_creation_time,
                                      senate_nontox_funds, sum)
senate_nontox_funds_plotting <- senate_nontox_funds_plotting[!grepl("2018", 
                                                                    senate_nontox_funds_plotting$ad_creation_time),]

# HR Toxic
## Date format was lost -- will have to re-convert from character
library(scales)
house_tox_funds_ts <- ggplot(hr_tox_funds_plotting, aes(x = ad_creation_time,
                                                        y = spend_upper)) +
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
  scale_x_discrete(breaks = c("2019-02", "2019-05", "2019-08", "2019-11",
                              "2020-02", "2020-05", "2020-08", "2020-11")) +
  scale_y_continuous(labels = unit_format(unit = "M", scale = 1e-6))# +
house_tox_funds_ts

# HR Nontoxic
house_nontox_funds_ts <- ggplot(hr_nontox_funds_plotting, 
                                aes(x = ad_creation_time,
                                    y = spend_upper)) +
  theme_bw() +
  coord_cartesian(ylim = c(0, 20000000)) +
  geom_bar(stat="identity") +
  labs(x = "Initial Distribution Date", y = "Total Funds Spent (Upper Estimate)") +
  ggtitle("House Nontoxic Advertisements") +
  theme(
    axis.text.x = element_text(angle = 30, hjust = 1),
    plot.title = element_text(hjust = 0.5)
  ) +
  scale_x_discrete(breaks = c("2019-02", "2019-05", "2019-08", "2019-11",
                              "2020-02", "2020-05", "2020-08", "2020-11")) +
  scale_y_continuous(labels = unit_format(unit = "M", scale = 1e-6))# +
house_nontox_funds_ts

# Senate Toxic
senate_tox_funds_ts <- ggplot(senate_tox_funds_plotting, 
                              aes(x = ad_creation_time,
                                  y = spend_upper)) +
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
  scale_x_discrete(breaks = c("2019-02", "2019-05", "2019-08", "2019-11",
                              "2020-02", "2020-05", "2020-08", "2020-11")) +
  scale_y_continuous(labels = unit_format(unit = "M", scale = 1e-6))# +
senate_tox_funds_ts

# Senate Nontoxic
senate_nontox_funds_ts <- ggplot(senate_nontox_funds_plotting, 
                                 aes(x = ad_creation_time,
                                     y = spend_upper)) +
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
  scale_x_discrete(breaks = c("2019-02", "2019-05", "2019-08", "2019-11",
                              "2020-02", "2020-05", "2020-08", "2020-11")) +
  scale_y_continuous(labels = unit_format(unit = "M", scale = 1e-6))# +
senate_nontox_funds_ts
# Arranging figure
funds_toxicity <- ggarrange(house_tox_funds_ts,
                            house_nontox_funds_ts,
                            senate_tox_funds_ts,
                            senate_nontox_funds_ts,
                            ncol = 2, nrow = 2)
funds_toxicity <- annotate_figure(
  funds_toxicity, top = text_grob("Total Funds Spent on Ads",
                             color = "black", face = "bold",
                             size = 14
  ))
funds_toxicity

ggsave("fig/funds_toxicity_ts.pdf", plot = funds_toxicity, 
       width = 10, height = 8)

# Weighting 2: Toxicity and impressions ========================================

## Because of the fact that there are some NAs in the impressions_upper column
## but no NAs in the impressions_lower, we'll use impressions_lower.
## The maximum value in impressions_upper is 999999...I take this to mean that 
## the NAs there are ads with predicted upper impressions >= 1 million

# HR Toxic:
hr_tox_imps <- house_tox %>% 
  group_by(ad_creation_time) %>%
  summarize(impressions_lower)
hr_tox_imps_plotting <- aggregate(impressions_lower ~ ad_creation_time,
                                   hr_tox_imps, sum)
hr_tox_imps_plotting[nrow(hr_tox_imps_plotting) + 1,] <- c("2020-12", 0)
hr_tox_imps_plotting$impressions_lower <- as.numeric(hr_tox_imps_plotting$impressions_lower)

# HR Nontoxic
hr_nontox_imps <- house_nontox %>% 
  group_by(ad_creation_time) %>%
  summarize(impressions_lower)
hr_nontox_imps_plotting <- aggregate(impressions_lower ~ ad_creation_time,
                                      hr_nontox_imps, sum)
hr_nontox_imps_plotting <- hr_nontox_imps_plotting[!grepl("2018", 
                                                          hr_nontox_imps_plotting$ad_creation_time),]

# Senate Toxic:
senate_tox_imps <- senate_tox %>% 
  group_by(ad_creation_time) %>%
  summarize(impressions_lower)
senate_tox_imps_plotting <- aggregate(impressions_lower ~ ad_creation_time,
                                       senate_tox_imps, sum)
senate_tox_imps_plotting <- senate_tox_imps_plotting[!grepl("2018", 
                                                            senate_tox_imps_plotting$ad_creation_time),]

# senate Nontoxic
senate_nontox_imps <- senate_nontox %>% 
  group_by(ad_creation_time) %>%
  summarize(impressions_lower)
senate_nontox_imps_plotting <- aggregate(impressions_lower ~ ad_creation_time,
                                          senate_nontox_imps, sum)
senate_nontox_imps_plotting <- senate_nontox_imps_plotting[!grepl("2018", 
                                                                  senate_nontox_imps_plotting$ad_creation_time),]

# HR Toxic
house_tox_imps_ts <- ggplot(hr_tox_imps_plotting, 
                            aes(x = ad_creation_time,
                                y = impressions_lower)) +
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
  scale_x_discrete(breaks = c("2019-02", "2019-05", "2019-08", "2019-11",
                              "2020-02", "2020-05", "2020-08", "2020-11")) +
  scale_y_continuous(labels = unit_format(unit = "M", scale = 1e-6))# +
house_tox_imps_ts
# HR Nontoxic
house_nontox_imps_ts <- ggplot(hr_nontox_imps_plotting, 
                               aes(x = ad_creation_time,
                                   y = impressions_lower)) +
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
  scale_x_discrete(breaks = c("2019-02", "2019-05", "2019-08", "2019-11",
                              "2020-02", "2020-05", "2020-08", "2020-11")) +
  scale_y_continuous(labels = unit_format(unit = "M", scale = 1e-6))# +
house_nontox_imps_ts
# Senate Toxic
senate_tox_imps_ts <- ggplot(senate_tox_imps_plotting, 
                             aes(x = ad_creation_time,
                                 y = impressions_lower)) +
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
  scale_x_discrete(breaks = c("2019-02", "2019-05", "2019-08", "2019-11",
                              "2020-02", "2020-05", "2020-08", "2020-11")) +
  scale_y_continuous(labels = unit_format(unit = "M", scale = 1e-6))# +
senate_tox_imps_ts
# Senate Nontoxic
senate_nontox_imps_ts <- ggplot(senate_nontox_imps_plotting, 
                                aes(x = ad_creation_time,
                                    y = impressions_lower)) +
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
  scale_x_discrete(breaks = c("2019-02", "2019-05", "2019-08", "2019-11",
                              "2020-02", "2020-05", "2020-08", "2020-11")) +
  scale_y_continuous(labels = unit_format(unit = "M", scale = 1e-6))# +
senate_nontox_imps_ts

# Arranging figure
imps_toxicity <- ggarrange(house_tox_imps_ts,
                            house_nontox_imps_ts,
                            senate_tox_imps_ts,
                            senate_nontox_imps_ts,
                            ncol = 2, nrow = 2)
imps_toxicity <- annotate_figure(
  imps_toxicity, top = text_grob("Total Impressions of Ads",
                                  color = "black", face = "bold",
                                  size = 14
  ))
imps_toxicity

ggsave("fig/imps_toxicity_ts.pdf", plot = imps_toxicity, 
       width = 10, height = 8)

# Illustrative Percentages =====================================================

# House
## Percent of Ads that are Toxic
nrow(house_tox) / (nrow(house_tox) + nrow(house_nontox))
### 12.682% of House ads are toxic
## Percent of Funds that go to Toxic ads
sum(hr_tox_funds$spend_upper) / (sum(hr_tox_funds$spend_upper) + 
                                   sum(hr_nontox_funds$spend_upper))
### 17.597% of funding goes to toxic ads
## Percent of Impressions that go to Toxic Ads
sum(hr_tox_imps$impressions_lower) / (sum(hr_tox_imps$impressions_lower) + 
                                   sum(hr_nontox_imps$impressions_lower))
### 16.749% of impressions go to Toxic ads

# Senate
## Percent of Ads that are Toxic
nrow(senate_tox) / (nrow(senate_tox) + nrow(senate_nontox))
## 12,136/92,746
### 13.085% of Senate ads (for senators running in 2020) are toxic
## Percent of Funds that go to Toxic ads
sum(senate_tox_funds$spend_upper) / (sum(senate_tox_funds$spend_upper) + 
                                   sum(senate_nontox_funds$spend_upper))
### 15.716% of funding goes to toxic ads
## Percent of Impressions that go to Toxic Ads
sum(senate_tox_imps$impressions_lower) / (
  sum(senate_tox_imps$impressions_lower) + 
    sum(senate_nontox_imps$impressions_lower))
### 14.936% of impressions go to Toxic ads

# Trump Mentions and Toxicity ==================================================

# Are ads that mention Trump more toxic than ads that don't?
# We'll need fb_unique for this -- go by party, chamber

load(here("data", "tidy", "fb_unique.Rda"))

# Manual Fixes for fb_unique Party =============================================

# There are some NAs in the party variable... let's fix them now.
# The next three lines, generating unique_parties, were used for reference.
unique_parties <- cbind(fb_unique$senate$candidate, fb_unique$senate$party)
unique_parties <- as.data.frame(unique_parties )
unique_parties <- unique(unique_parties)

fb_unique$senate <- fb_unique$senate %>%
  mutate(
    party = case_when(
      candidate == "deb fischer" ~ "REPUBLICAN",
      candidate == "john barrasso" ~ "REPUBLICAN",
      candidate == "rick scott" ~ "REPUBLICAN",
      candidate == "bernie sanders" ~ "INDEPENDENT",
      candidate == "patty murray" ~ "DEMOCRAT",
      candidate == "martin heinrich" ~ "DEMOCRAT",
      candidate == "mazie hirono" ~ "DEMOCRAT",
      candidate == "marsha blackburn" ~ "REPUBLICAN",
      candidate == "patrick leahy" ~ "DEMOCRAT",
      candidate == "john kennedy" ~ "REPUBLICAN",
      candidate == "josh hawley" ~ "REPUBLICAN",
      candidate == "mike braun" ~ "REPUBLICAN",
      candidate == "chris murphy" ~ "DEMOCRAT",
      candidate == "mike lee" ~ "REPUBLICAN",
      candidate == "tammy duckworth" ~ "DEMOCRAT",
      candidate == "richard blumenthal" ~ "DEMOCRAT",
      candidate == "tim scott" ~ "REPUBLICAN",
      candidate == "elizabeth warren" ~ "DEMOCRAT",
      candidate == "todd young" ~ "REPUBLICAN",
      candidate == "jacky rosen" ~ "DEMOCRAT",   
      candidate == "kyrsten sinema" ~ "DEMOCRAT",
      candidate == "rand paul" ~ "REPUBLICAN",
      candidate == "ron wyden" ~ "DEMOCRAT",
      candidate == "michael bennet" ~ "DEMOCRAT",
      candidate == "brian schatz" ~ "DEMOCRAT",
      candidate == "marco rubio" ~ "REPUBLICAN",
      candidate == "sherrod brown" ~ "DEMOCRAT",
      candidate == "john thune" ~ "REPUBLICAN",
      candidate == "jon tester" ~ "DEMOCRAT",
      candidate == "roy blunt" ~ "REPUBLICAN",      
      candidate == "roger wicker" ~ "REPUBLICAN",
      candidate == "bob casey" ~ "DEMOCRAT",
      candidate == "kirsten gillibrand" ~ "DEMOCRAT",
      candidate == "ted cruz" ~ "REPUBLICAN",
      candidate == "chris van hollen" ~ "DEMOCRAT",
      candidate == "tammy baldwin" ~ "DEMOCRAT",
      candidate == "amy klobuchar" ~ "DEMOCRAT",
      candidate == "chuck schumer" ~ "DEMOCRAT",
      candidate == "debbie stabenow" ~ "DEMOCRAT",
      candidate == "pat toomey" ~ "REPUBLICAN",
      TRUE ~ party
    )
  )

### House side -- Trump Toxicity ===============================================

house_unique <- merge(
  x = fb_house,
  y = fb_unique$house[, c("fb_ad_library_id", "ad_creative_body", 
                          "word_trump", "party"
  )],
  by = c("fb_ad_library_id", "ad_creative_body"), all.x = TRUE
)
assert_that(!any(is.na(house_unique$word_trump))) # True
assert_that(!any(is.na(house_unique$party))) # True
#assert_that(!any(house_unique$party == "INDEPENDENT")) # NOT true
house_unique <- house_unique[house_unique$party != "INDEPENDENT", ]
# 51,304 to 51,149
assert_that(!any(house_unique$party == "INDEPENDENT")) # True

house_gop_trump <- house_unique[house_unique$word_trump == 1 & 
                                  house_unique$party == "REPUBLICAN", ]
house_gop_notrump <- house_unique[house_unique$word_trump == 0 & 
                                  house_unique$party == "REPUBLICAN", ]
house_dem_trump <- house_unique[house_unique$word_trump == 1 & 
                                  house_unique$party == "DEMOCRAT", ]
house_dem_notrump <- house_unique[house_unique$word_trump == 0 & 
                                    house_unique$party == "DEMOCRAT", ]
assert_that(nrow(house_unique) == sum(nrow(house_gop_notrump),
                                      nrow(house_gop_trump),
                                      nrow(house_dem_notrump),
                                      nrow(house_dem_trump)
                                      )
            ) # True

# House Republican ads that mention Trump
mean(house_gop_trump$toxicity) 
## 0.149
quantile(house_gop_trump$toxicity)
## 0%        25%        50%        75%       100% 
## 0.02042444 0.07955136 0.11062716 0.16951741 0.94808560 

# House Republican ads that don't mention Trump
mean(house_gop_notrump$toxicity) 
## 0.126
quantile(house_gop_notrump$toxicity)
## 0%          25%          50%          75%         100% 
## 0.0006077258 0.0630457630 0.0918971150 0.1508413650 0.9593848600 

# House Democrat ads that mention Trump
mean(house_dem_trump$toxicity)
# 0.159
quantile(house_dem_trump$toxicity)
## 0%        25%        50%        75%       100% 
## 0.02162239 0.08445841 0.11997437 0.18356327 0.75283150 

# House Democrat ads that don't mention Trump
mean(house_dem_notrump$toxicity)
# 0.102
quantile(house_dem_notrump$toxicity)
# 0%          25%          50%          75%         100% 
# 3.799359e-05 0.05551384 0.07691913 0.1138114 0.9560496 

### Plotting House =============================================================

house_gop_trump$trump <- "Republican, Mention Trump"
house_gop_notrump$trump <- "Republican, Don't Mention Trump"
house_dem_trump$trump <- "Democrat, Mention Trump"
house_dem_notrump$trump <- "Democrat, Don't Mention Trump"

houseplot <- as.data.frame(rbind(house_gop_trump, house_gop_notrump,
                                 house_dem_trump, house_dem_notrump))

house_plot <- ggplot(houseplot) +
  aes(x = trump, y = toxicity) +
  geom_boxplot(fill = "#0c4c8a") +
  coord_cartesian(ylim = c(0, 1)) +
  labs(x = "Party and Trump Mentions", y = "Toxicity") +
  ggtitle("House") +
  scale_x_discrete(guide = guide_axis(n.dodge=2))+
  theme_minimal() +
  theme(
    text = element_text(size=10),
    plot.title = element_text(hjust = 0.5)#,
    #axis.text.x = element_text(angle = 30, hjust = 1)
  )

### House difference-in-means tests ============================================

# Law of large numbers, central limit theorem should in theory make the t-test
# a viable difference-in-means test here...

t.test(house_gop_trump$toxicity, house_gop_notrump$toxicity)
t.test(house_dem_trump$toxicity, house_dem_notrump$toxicity)

### Senate side -- Trump toxicity ==============================================

senate_unique <- merge(
  x = fb_senate,
  y = fb_unique$senate[, c("fb_ad_library_id", "ad_creative_body", 
                          "word_trump", "party"
  )],
  by = c("fb_ad_library_id", "ad_creative_body"), all.x = TRUE
)
assert_that(!any(is.na(senate_unique$word_trump))) # True
assert_that(!any(is.na(senate_unique$party))) # True, thanks to earlier edits
#assert_that(!any(senate_unique$party == "INDEPENDENT")) # NOT true
senate_unique <- senate_unique[senate_unique$party != "INDEPENDENT", ]
# 28,796 to 27,059...that's Bernie, no doubt
assert_that(!any(senate_unique$party == "INDEPENDENT")) # True

senate_gop_trump <- senate_unique[senate_unique$word_trump == 1 & 
                                  senate_unique$party == "REPUBLICAN", ]
senate_gop_notrump <- senate_unique[senate_unique$word_trump == 0 & 
                                    senate_unique$party == "REPUBLICAN", ]
senate_dem_trump <- senate_unique[senate_unique$word_trump == 1 & 
                                  senate_unique$party == "DEMOCRAT", ]
senate_dem_notrump <- senate_unique[senate_unique$word_trump == 0 & 
                                    senate_unique$party == "DEMOCRAT", ]
assert_that(nrow(senate_unique) == sum(nrow(senate_gop_notrump),
                                      nrow(senate_gop_trump),
                                      nrow(senate_dem_notrump),
                                      nrow(senate_dem_trump)
)
) # True

# senate Republican ads that mention Trump
mean(senate_gop_trump$toxicity) 
## 0.143
quantile(senate_gop_trump$toxicity)
## 0%        25%        50%        75%       100% 
## 0.02659953 0.08040548 0.11116875 0.17066476 0.79451610 

# senate Republican ads that don't mention Trump
mean(senate_gop_notrump$toxicity) 
## 0.128
quantile(senate_gop_notrump$toxicity)
## 0%          25%          50%          75%         100% 
## 0.0007199546 0.0650521625 0.0941707200 0.1510069025 0.8749342000 

# senate Democrat ads that mention Trump
mean(senate_dem_trump$toxicity)
# 0.165
quantile(senate_dem_trump$toxicity)
## 0%         25%         50%         75%        100% 
## 0.007787015 0.089007342 0.122776896 0.192136780 0.915652800 

# senate Democrat ads that don't mention Trump
mean(senate_dem_notrump$toxicity)
# 0.097
quantile(senate_dem_notrump$toxicity)
## 0%          25%          50%          75%         100% 
## 0.0002664225 0.0567164280 0.0778784900 0.1105391300 0.9326348300 

### Plotting Senate ============================================================

senate_gop_trump$trump <- "Republican, Mention Trump"
senate_gop_notrump$trump <- "Republican, Don't Mention Trump"
senate_dem_trump$trump <- "Democrat, Mention Trump"
senate_dem_notrump$trump <- "Democrat, Don't Mention Trump"

senateplot <- as.data.frame(rbind(senate_gop_trump, senate_gop_notrump,
                                 senate_dem_trump, senate_dem_notrump))

senate_plot <- ggplot(senateplot) +
  aes(x = trump, y = toxicity) +
  geom_boxplot(fill = "#0c4c8a") +
  coord_cartesian(ylim = c(0, 1)) +
  labs(x = "Party and Trump Mentions", y = "Toxicity") +
  ggtitle("Senate") +
  scale_x_discrete(guide = guide_axis(n.dodge=2))+
  theme_minimal() +
  theme(
    text = element_text(size=10),
    plot.title = element_text(hjust = 0.5)#,
    #axis.text.x = element_text(angle = 30, hjust = 1)
  )


trump_toxicity <- ggarrange(house_plot, senate_plot,
                            ncol = 2, nrow = 1)
trump_toxicity <- annotate_figure(
  trump_toxicity, 
  top = text_grob("Advertisement Toxicity by Partisanship and Trump Mentions",
                                  color = "black", face = "bold",
                                  size = 14
  ))
trump_toxicity

ggsave("fig/trump_toxicity.pdf", plot = trump_toxicity, 
       width = 10, height = 8)


### Senate difference-in-means tests (T test) ==================================

# Law of large numbers, central limit theorem should in theory make the t-test
# a viable difference-in-means test here...

t.test(senate_gop_trump$toxicity, senate_gop_notrump$toxicity)
t.test(senate_dem_trump$toxicity, senate_dem_notrump$toxicity)


### Weighting donor/voter by toxicity for Warnock and Loeffler =================

# Merging weighting variables into fb_senate ===================================

fb_senate <- read_rds("data/tidy/fb_perspective_Senate_20220426.RDS")
load(here("data", "tidy", "fb_unique.Rda"))
unique <- fb_unique

# Sanity checks, before we get started...
assert_that(!any(is.na(fb_senate$ad_creative_body)))
assert_that(!any(is.na(unique$senate$ad_creative_body)))
senate_target_weights <- merge(
  x = fb_senate,
  y = unique$senate[, c("fb_ad_library_id", "ad_creative_body", 
                        "financial"
  )],
  by = c("fb_ad_library_id", "ad_creative_body"), all.x = TRUE
)
loeffler <- senate_target_weights[senate_target_weights$candidate == 
                                    'kelly loeffler', ]
warnock <- senate_target_weights[senate_target_weights$candidate == 
                                    'raphael warnock', ]
loeffler_donors <- loeffler[loeffler$financial == 'Donor-targeting', ]
loeffler_voters <- loeffler[loeffler$financial == 'Voter-targeting', ]
warnock_donors <- warnock[warnock$financial == 'Donor-targeting', ]
warnock_voters <- warnock[warnock$financial == 'Voter-targeting', ]

mean(loeffler_donors$toxicity) #0.1493359
mean(loeffler_voters$toxicity) # 0.1317107
mean(warnock_donors$toxicity) # 0.2353634
mean(warnock_voters$toxicity) # 0.1431848