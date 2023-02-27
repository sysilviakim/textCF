library(tidyverse)
library(here)
library(glue)
library(quanteda)
## devtools::install_github("kbenoit/quanteda.dictionaries")
library(quanteda.dictionaries)

NRC <- lingmatch::read.dic(here("data", "raw", "dictionaries", "NRC.dic"))
NRC <- dictionary(NRC)

## Text is in "ad_creative_body"
load(here("data", "tidy", "fb_unique.Rda"))

# Confirming that we have 56,585 unique ads
# (our paper as of Feb. 26, 2023 says 16368 + 40217 = 56585)
fb_unique %>%
  imap_dfr(~ .x %>% count(party)) %>%
  filter(party %in% c("DEMOCRAT", "REPUBLICAN")) %>%
  pull(n) %>%
  sum()

# Emotionally charged rhetoric =================================================
# NOTE: running quanteda::corpus() is sufficient and additional pre-processing
# is not needed in this context because liwcalike() will lowercase texts
# by default
fb_corpus_list <- list(
  senate_rep_f = fb_unique$senate %>%
    filter(party == "REPUBLICAN" & financial == "Donor-targeting") %>%
    corpus(., text_field = "ad_creative_body"),
  senate_rep_n = fb_unique$senate %>%
    filter(party == "REPUBLICAN" & financial == "Voter-targeting") %>%
    corpus(., text_field = "ad_creative_body"),
  senate_dem_f = fb_unique$senate %>%
    filter(party == "DEMOCRAT" & financial == "Donor-targeting") %>%
    corpus(., text_field = "ad_creative_body"),
  senate_dem_n = fb_unique$senate %>%
    filter(party == "DEMOCRAT" & financial == "Voter-targeting") %>%
    corpus(., text_field = "ad_creative_body"),
  house_rep_f = fb_unique$house %>%
    filter(party == "REPUBLICAN" & financial == "Donor-targeting") %>%
    corpus(., text_field = "ad_creative_body"),
  house_rep_n = fb_unique$house %>%
    filter(party == "REPUBLICAN" & financial == "Voter-targeting") %>%
    corpus(., text_field = "ad_creative_body"),
  house_dem_f = fb_unique$house %>%
    filter(party == "DEMOCRAT" & financial == "Donor-targeting") %>%
    corpus(., text_field = "ad_creative_body"),
  house_dem_n = fb_unique$house %>%
    filter(party == "DEMOCRAT" & financial == "Voter-targeting") %>%
    corpus(., text_field = "ad_creative_body")
)

lexi_list <- fb_corpus_list %>%
  map(~ liwcalike(.x, dictionary = NRC))

temp2 <- lexi_list %>%
  imap_dfr(~ .x %>% select(Dic, anger, disgust, fear), .id = "type") %>%
  mutate(
    party = case_when(
      grepl("rep", type) ~ "Republican", TRUE ~ "Democrat"
    ),
    financial = case_when(
      grepl("_f", type) ~ "Donor-targeting", TRUE ~ "Voter-targeting"
    )
  )

temp2 %>%
  group_by(financial) %>%
  summarise(
    mean_anger = mean(anger / Dic, na.rm = TRUE),
    mean_disgust = mean(disgust / Dic, na.rm = TRUE),
    mean_fear = mean(fear / Dic, na.rm = TRUE),
    se_anger = sd(anger / Dic, na.rm = TRUE) / sqrt(n()),
    se_disgust = sd(disgust / Dic, na.rm = TRUE) / sqrt(n()),
    se_fear = sd(fear / Dic, na.rm = TRUE) / sqrt(n()),
    any_anger = mean(anger > 0),
    any_disgust = mean(disgust > 0),
    any_fear = mean(fear > 0),
  ) %>%
  ungroup()

#   financial       mean_anger mean_disgust mean_fear se_anger
#   <chr>                <dbl>        <dbl>     <dbl>    <dbl>
# 1 Donor-targeting     0.0746       0.0393    0.0747 0.000459
# 2 Voter-targeting     0.0588       0.0376    0.0644 0.000454
#   se_disgust  se_fear any_anger any_disgust any_fear
#        <dbl>    <dbl>     <dbl>       <dbl>    <dbl>
# 1   0.000371 0.000499     0.600       0.400    0.576
# 2   0.000414 0.000535     0.496       0.345    0.475

# Binary for presence of enotional words
temp2$anyanger <- as.numeric(temp2$anger > 0)
temp2$anydisgust <- as.numeric(temp2$disgust > 0)
temp2$anyfear <- as.numeric(temp2$fear > 0)

temp2$anyOfAngerFearDisgust <-
  as.numeric(temp2$anyanger == 1 | temp2$anydisgust == 1 | temp2$anyfear == 1)
temp2$allOfAngerFearDisgust <-
  as.numeric(temp2$anyanger == 1 & temp2$anydisgust == 1 & temp2$anyfear == 1)

temp2 %>%
  count(anyOfAngerFearDisgust, anyanger, anyfear, anydisgust) %>%
  mutate(perc = n / sum(n) * 100)


# Paper text: "We also observe that 29.3\% of ads directed at donors
# contain words linked to all three emotions
# (anger, fear and disgust) while only 20.7\% of voter-targeting ads
# lean simultaneously into these emotions."
temp2 %>%
  group_by(financial) %>%
  count(allOfAngerFearDisgust) %>%
  mutate(perc = n / sum(n) * 100)

# Paper text: "a word linked to at least of of these emotions is present
# in 74.1\% of donor-targeting ads,
# and in 68.4\% of ads aimed at voters."
temp2 %>%
  group_by(financial) %>%
  count(anyOfAngerFearDisgust) %>%
  mutate(perc = n / sum(n) * 100)

# Full distribution:
temp2 %>%
  group_by(financial) %>%
  count(anyOfAngerFearDisgust, anyanger, anyfear, anydisgust) %>%
  mutate(perc = n / sum(n) * 100)

# Paper text: "Pooling across chambers, we observe that donor-targeting ads
# contain more fear-related words
# than voter-targeting ads (7.5\% vs. 6.4\%, t-statistic = 13.9)".
t.test(
  temp2$fear[temp2$financial == "Donor-targeting"] /
    temp2$Dic[temp2$financial == "Donor-targeting"],
  temp2$fear[temp2$financial == "Voter-targeting"] /
    temp2$Dic[temp2$financial == "Voter-targeting"]
)

# Paper text: "words associated with anger are more prevalent in donor-targeting
# ads compared to voter-targeting ads (7.5\% vs 5.9\%, t-statistic = 24.1)".
t.test(
  temp2$anger[temp2$financial == "Donor-targeting"] /
    temp2$Dic[temp2$financial == "Donor-targeting"],
  temp2$anger[temp2$financial == "Voter-targeting"] /
    temp2$Dic[temp2$financial == "Voter-targeting"]
)

# disgust
t.test(
  temp2$disgust[temp2$financial == "Donor-targeting"] /
    temp2$Dic[temp2$financial == "Donor-targeting"],
  temp2$disgust[temp2$financial == "Voter-targeting"] /
    temp2$Dic[temp2$financial == "Voter-targeting"]
)

# Binary for any presence:
# anger
t.test(
  temp2$anyanger[temp2$financial == "Donor-targeting"],
  temp2$anyanger[temp2$financial == "Voter-targeting"]
)

# disgust
t.test(
  temp2$anydisgust[temp2$financial == "Donor-targeting"],
  temp2$anydisgust[temp2$financial == "Voter-targeting"]
)

# fear
t.test(
  temp2$anyfear[temp2$financial == "Donor-targeting"],
  temp2$anyfear[temp2$financial == "Voter-targeting"]
)


# PID differnces [not used in the text]

fb_pid_corpus_list <- list(
  republicans = fb_unique %>%
    imap_dfr(~ .x %>% filter(party == "REPUBLICAN")) %>%
    corpus(., text_field = "ad_creative_body"),
  democrats = fb_unique %>%
    imap_dfr(~ .x %>% filter(party == "DEMOCRAT")) %>%
    corpus(., text_field = "ad_creative_body")
)

lexi_pid_list <- fb_pid_corpus_list %>%
  map(~ liwcalike(.x, dictionary = NRC))

lixi_pid_unlisted <- lexi_pid_list %>%
  imap_dfr(~ .x %>% select(Dic, anger, disgust, fear), .id = "pid")

# Democrats use more anger than Republicans; Republicans use more fear...

# anger
t.test(
  lixi_pid_unlisted$anger[lixi_pid_unlisted$pid == "republicans"] /
    lixi_pid_unlisted$Dic[lixi_pid_unlisted$pid == "republicans"],
  lixi_pid_unlisted$anger[lixi_pid_unlisted$pid == "democrats"] /
    lixi_pid_unlisted$Dic[lixi_pid_unlisted$pid == "democrats"]
)

# disgust
t.test(
  lixi_pid_unlisted$disgust[lixi_pid_unlisted$pid == "republicans"] /
    lixi_pid_unlisted$Dic[lixi_pid_unlisted$pid == "republicans"],
  lixi_pid_unlisted$disgust[lixi_pid_unlisted$pid == "democrats"] /
    lixi_pid_unlisted$Dic[lixi_pid_unlisted$pid == "democrats"]
)

# fear
t.test(
  lixi_pid_unlisted$fear[lixi_pid_unlisted$pid == "republicans"] /
    lixi_pid_unlisted$Dic[lixi_pid_unlisted$pid == "republicans"],
  lixi_pid_unlisted$fear[lixi_pid_unlisted$pid == "democrats"] /
    lixi_pid_unlisted$Dic[lixi_pid_unlisted$pid == "democrats"]
)
