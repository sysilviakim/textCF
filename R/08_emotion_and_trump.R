source(here::here("R", "utilities.R"))
options(digits = 3, scipen = 999)

# Load data ====================================================================
## Party is already filtered and labeled; no need to repeat the process
fb_unique <- read_rds(here("data", "tidy", "toxicity.RDS")) %>%
  ## Adjust candidate labels so that nchar is the same
  rowwise() %>%
  mutate(
    candidate = str_pad(
      simple_cap(tolower(candidate)),
      side = "left",
      width = max(nchar(.$candidate))
    )
  )

## Note that candidate is the group-level marker, not page_name
## e.g., Luke Letlow For Congress != Luke Letlow, but same candidate
assert_that(
  length(unique(fb_unique$page_name)) != ## 842
    length(unique(fb_unique$candidate)) ## 720
)

# Create party x target x chamber split corpuses ===============================
fb_corpus_list <- fb_unique %>%
  group_by(chamber, financial, party)
temp <- group_keys(fb_corpus_list) %>%
  unite(group_name, chamber, party, financial, sep = ", ") %>%
  .$group_name
fb_corpus_list <- fb_corpus_list %>%
  group_split() %>%
  set_names(., temp) %>%
  map(~ corpus(.x, text_field = "ad_creative_body"))

# Emotionally charged rhetoric =================================================
lexi_list <- fb_corpus_list %>%
  map(~ liwcalike(.x, dictionary = NRC))
save(lexi_list, file = here("output", "lexi_list.Rda"))

emotion_df <- lexi_list %>%
  imap(~ .x %>% select(Dic, anger, disgust, fear)) %>%
  bind_rows(.id = "type") %>%
  separate(type, into = c("chamber", "party", "financial"), sep = ", ") %>%
  party_factor(., outvar = "type")

emotion_summ <- emotion_df %>%
  group_by(type, chamber) %>%
  summ_emo()

## In-text descriptives --------------------------------------------------------
emotion_df %>%
  group_by(financial) %>%
  summ_emo() %>%
  print(width = 1e6)

#   financial       mean_anger mean_disgust mean_fear se_anger
#   <chr>                <dbl>        <dbl>     <dbl>    <dbl>
# 1 Donor-targeting     0.0746       0.0393    0.0747 0.000459
# 2 Voter-targeting     0.0588       0.0376    0.0644 0.000454
#   se_disgust  se_fear any_anger any_disgust any_fear
#        <dbl>    <dbl>     <dbl>       <dbl>    <dbl>
# 1   0.000371 0.000499     0.600       0.400    0.576
# 2   0.000414 0.000535     0.496       0.345    0.475

emotion_df <- emotion_df %>%
  mutate(
    any_anger = as.numeric(anger > 0),
    any_disgust = as.numeric(disgust > 0),
    any_fear = as.numeric(fear > 0)
  ) %>%
  mutate(
    any_negative = case_when(
      any_anger == 1 | any_disgust == 1 | any_fear == 1 ~ 1,
      TRUE ~ 0
    ),
    all_negative = case_when(
      any_anger == 1 & any_disgust == 1 & any_fear == 1 ~ 1,
      TRUE ~ 0
    )
  )

# 29.3\% of donor-targeting ads linked to all three emotions
# 20.7\% of voter-targeting ads
ttest_short(emotion_df, "all_negative", emo_prevalence = FALSE)

# 74.1\% of donor-targeting ads linked to any of the three emotions
# 68.4\% of voter-targeting ads
ttest_short(emotion_df, "any_negative", emo_prevalence = FALSE)

## In-text t-tests -------------------------------------------------------------
by_chamber_party <- emotion_df %>%
  group_by(chamber, party)
temp <- group_keys(by_chamber_party) %>%
  unite(group_name, chamber, party, sep = ", ") %>%
  .$group_name
by_chamber_party <- by_chamber_party %>%
  group_split() %>%
  set_names(., temp)

## anger: 7.5\% vs. 5.9\%, t-statistic = 24.1
ttest_short(emotion_df, "anger")
ttest_short(by_chamber_party$`House, Democrat`, "anger")
ttest_short(by_chamber_party$`House, Republican`, "anger")
ttest_short(by_chamber_party$`Senate, Democrat`, "anger")
ttest_short(by_chamber_party$`Senate, Republican`, "anger")

## fear: 7.5\% vs. 6.4\%, t-statistic = 13.9
ttest_short(emotion_df, "fear")
ttest_short(by_chamber_party$`House, Democrat`, "fear")
ttest_short(by_chamber_party$`House, Republican`, "fear")
ttest_short(by_chamber_party$`Senate, Democrat`, "fear")
ttest_short(by_chamber_party$`Senate, Republican`, "fear")

## disgust: 3.9\% vs. 3.8\%, t-statistic = 2.96
ttest_short(emotion_df, "disgust")
ttest_short(by_chamber_party$`House, Democrat`, "disgust")
ttest_short(by_chamber_party$`House, Republican`, "disgust")
ttest_short(by_chamber_party$`Senate, Democrat`, "disgust")
ttest_short(by_chamber_party$`Senate, Republican`, "disgust")

## Binary for any presence
## anger: 2.9\% vs. 1.9\%, t-statistic = 44.1
ttest_short(emotion_df, "any_anger")

## fear: 2.8\% vs. 2.0\%, t-statistic = 33.3
ttest_short(emotion_df, "any_fear")

## disgust: 1.9\% vs. 1.4\%, t-statistic = 20.3
ttest_short(emotion_df, "any_disgust")

## Figure export ---------------------------------------------------------------
pdf(here("fig", "anger_by_type_chamber.pdf"), width = 6, height = 2.8)
print(
  fb_mention_plot(
    emotion_summ %>% rename(Party = type),
    xvar = "mean_anger", se = "se_anger", xlab = ""
  ) +
    scale_x_continuous(limits = c(0, 0.09), labels = scales::percent)
)
dev.off()

pdf(here("fig", "disgust_by_type_chamber.pdf"), width = 6, height = 2.8)
print(
  fb_mention_plot(
    emotion_summ %>% rename(Party = type),
    xvar = "mean_disgust", se = "se_disgust", xlab = ""
  ) +
    scale_x_continuous(limits = c(0, 0.09), labels = scales::percent)
)
dev.off()

pdf(here("fig", "fear_by_type_chamber.pdf"), width = 6, height = 2.8)
print(
  fb_mention_plot(
    emotion_summ %>% rename(Party = type),
    xvar = "mean_fear", se = "se_fear", xlab = ""
  ) +
    scale_x_continuous(limits = c(0, 0.09), labels = scales::percent)
)
dev.off()

# Mention of Trump =============================================================
trump_df <- fb_unique %>%
  group_by(party, financial, chamber) %>%
  summarise(
    mean_trump = mean(word_trump),
    sd_trump = sd(word_trump),
    se_trump = sd_trump / sqrt(n())
  ) %>%
  ungroup() %>%
  party_factor(., outvar = "Party")

## Figure export ---------------------------------------------------------------
pdf(here("fig", "mention_trump_by_type_chamber.pdf"), width = 6, height = 2.8)
print(
  fb_mention_plot(
    trump_df,
    xvar = "mean_trump", se = "se_trump", xlab = "Mentions Trump"
  )
)
dev.off()

## In-text t-tests -------------------------------------------------------------
t.test(
  fb_unique %>%
    filter(party == "Republican" & financial == "Donor-targeting") %>%
    .$word_trump,
  fb_unique %>%
    filter(party == "Republican" & financial == "Voter-targeting") %>%
    .$word_trump
)

# Jan's PID difference code (unused in text) ===================================
lexi_pid <- fb_unique %>%
  group_by(party) %>%
  group_split() %>%
  `names<-`({.} %>% map(~ .x$party[1]) %>% unlist()) %>%
  map(~ corpus(.x, text_field = "ad_creative_body")) %>%
  map(~ liwcalike(.x, dictionary = NRC)) %>%
  imap(~ .x %>% select(Dic, anger, disgust, fear)) %>%
  bind_rows(.id = "party")

# anger
t.test(
  lexi_pid$anger[lexi_pid$party == "Republican"] /
    lexi_pid$Dic[lexi_pid$party == "Republican"],
  lexi_pid$anger[lexi_pid$party == "Democrat"] /
    lexi_pid$Dic[lexi_pid$party == "Democrat"]
)

# disgust
t.test(
  lexi_pid$disgust[lexi_pid$party == "Republican"] /
    lexi_pid$Dic[lexi_pid$party == "Republican"],
  lexi_pid$disgust[lexi_pid$party == "Democrat"] /
    lexi_pid$Dic[lexi_pid$party == "Democrat"]
)

# fear
t.test(
  lexi_pid$fear[lexi_pid$party == "Republican"] /
    lexi_pid$Dic[lexi_pid$party == "Republican"],
  lexi_pid$fear[lexi_pid$party == "Democrat"] /
    lexi_pid$Dic[lexi_pid$party == "Democrat"]
)
