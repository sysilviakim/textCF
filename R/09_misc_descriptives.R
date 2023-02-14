source(here::here("R", "utilities.R"))
options(digits = 3, scipen = 999)

# Import; will deal with descriptives ==========================================
load(here("data", "tidy", "merged_unique.Rda"))
load(here("data", "tidy", "merged_all.Rda"))

## This is the main dataset
df <- read_rds(here("data", "tidy", "toxicity.RDS")) %>%
  select(
    candidate, chamber, ad_creative_body, toxicity, party, inc, financial,
    everything()
  )

# Number of rows ===============================================================
nrow(df)
## 56585

table(df$chamber)
# House Senate
# 40217  16368

## Alexandria Ocasio-Cortez example
df %>%
  filter(candidate == "alexandria ocasio cortez") %>%
  nrow()
## 498

df_all %>%
  filter(candidate == "alexandria ocasio cortez") %>%
  nrow()
## 48169

# Summary of toxicity and top toxic posts ======================================
summary(df$toxicity)
mean(df$toxicity, na.rm = TRUE)

## By-type toxicity summary
df %>%
  group_by(financial) %>%
  summarise(toxicity = mean(toxicity, na.rm = TRUE))
# financial         toxicity
# <fct>                <dbl>
# 1 Voter-targeting   0.0533
# 2 Donor-targeting   0.0651

## By-party toxicity summary
df %>%
  group_by(party) %>%
  summarise(toxicity = mean(toxicity, na.rm = TRUE))
# party        toxicity
# <chr>           <dbl>
# 1 Democrat     0.0549
# 2 Republican   0.0656

## By-party top toxic posts
df %>%
  group_by(party) %>%
  arrange(desc(toxicity)) %>%
  slice_head(n = 5) %>%
  arrange(chamber, desc(toxicity)) %>%
  View()

## Party x financial
df %>%
  group_by(party, financial, chamber) %>%
  summarise(toxicity = mean(toxicity, na.rm = TRUE))

# Binary toxicity ==============================================================
df %>%
  group_by(financial) %>%
  mutate(toxic = case_when(toxicity > 0.5 ~ 1, TRUE ~ 0)) %>%
  summarise(toxic = mean(toxic, na.rm = TRUE) * 100)

df %>%
  group_by(chamber, financial) %>%
  mutate(toxic = case_when(toxicity > 0.5 ~ 1, TRUE ~ 0)) %>%
  summarise(toxic = mean(toxic, na.rm = TRUE) * 100)

df %>%
  group_by(chamber, party, financial) %>%
  mutate(toxic = case_when(toxicity > 0.5 ~ 1, TRUE ~ 0)) %>%
  summarise(toxic = mean(toxic, na.rm = TRUE) * 100)

# Impressions by party =========================================================
df %>%
  group_by(chamber) %>%
  summarise(
    impressions_lower = mean(min_impressions_lower, na.rm = TRUE),
    impressions_upper = mean(max_impressions_upper, na.rm = TRUE)
  )

# Pick some examples from random sample for Table 1 ============================
## Among these, picked the most illustrious
set.seed(123)
df %>%
  group_by(party, financial) %>%
  select(party, financial, toxicity, ad_creative_body, candidate, state) %>%
  sample_n(5) %>%
  View()

df %>%
  group_by(party, financial) %>%
  select(party, financial, toxicity, ad_creative_body, candidate, state) %>%
  sample_n(5) %>%
  View()

## Does length vary?
df %>%
  group_by(party, financial) %>%
  summarise(mean = mean(nchar(ad_creative_body)))

## Driven generally by Dem party: ~ 88 character of difference
df %>%
  group_by(financial) %>%
  summarise(mean = mean(nchar(ad_creative_body))) %>%
  .$mean %>%
  {
    .[2] - .[1]
  }

# Wordcloud by target x party ==================================================
## prep data
fb_corpus <- list(
  dem_donor = df %>% filter(grepl("Donor", financial) & party == "Democrat"),
  dem_voter = df %>% filter(grepl("Voter", financial) & party == "Democrat"),
  rep_donor = df %>% filter(grepl("Donor", financial) & party == "Republican"),
  rep_voter = df %>% filter(grepl("Voter", financial) & party == "Republican")
) %>%
  map(
    ~ corpus(
      .x$ad_creative_body,
      docvars = tibble(
        candidate = .x$candidate,
        chamber = .x$chamber
      )
    )
  )

fb_tokens <- fb_corpus %>%
  map(
    ~ tokens(.x) %>%
      tokens(
        remove_url = TRUE,
        remove_punct = TRUE,
        remove_symbols = TRUE,
        remove_numbers = TRUE,
        remove_separators = TRUE,
        include_docvars = TRUE
      ) %>%
      tokens_tolower() %>%
      tokens_remove(
        c(
          stopwords("english"),
          "may", "shall", "can", "must", "upon", "with", "without"
        )
      ) %>%
      tokens_remove(stopwords("spanish")) %>%
      tokens_remove(letters) %>%
      tokens_wordstem() %>%
      tokens_select(min_nchar = 3)
  )

fb_dfm <- fb_tokens %>% map(dfm)

## I'd like to directly do separate wordclouds rather than `comparison = TRUE`
## on top of grouped dfm
## Not saved as a ggplot2 object so must do ggsave for each one?
pdf(here("fig", "wordcloud_dem_donor.pdf"), width = 4, height = 2.5)
textplot_wordcloud(
  fb_dfm$dem_donor,
  max_words = 100, rotation = .25,
  color = RColorBrewer::brewer.pal(9, "Blues")[4:9]
)
dev.off()

pdf(here("fig", "wordcloud_dem_voter.pdf"), width = 4, height = 2.5)
textplot_wordcloud(
  fb_dfm$dem_voter,
  max_words = 100, rotation = .25,
  color = RColorBrewer::brewer.pal(9, "Blues")[4:9]
)
dev.off()

pdf(here("fig", "wordcloud_rep_donor.pdf"), width = 4, height = 2.5)
textplot_wordcloud(
  fb_dfm$rep_donor,
  max_words = 100, rotation = .2, ## I want "help" to be not rotated
  color = RColorBrewer::brewer.pal(9, "Blues")[4:9]
)
dev.off()

pdf(here("fig", "wordcloud_rep_voter.pdf"), width = 4, height = 2.5)
textplot_wordcloud(
  fb_dfm$rep_voter,
  max_words = 100, rotation = .25,
  color = RColorBrewer::brewer.pal(9, "Blues")[4:9]
)
dev.off()

# Loeffler vs. Warnock summaries ===============================================
df %>%
  filter(candidate == "kelly loeffler" | candidate == "raphael warnock") %>%
  group_by(candidate, financial) %>%
  summarise(toxicity = mean(toxicity))

# candidate       financial       toxicity
# <chr>           <fct>              <dbl>
# 1 kelly loeffler  Voter-targeting   0.0603
# 2 kelly loeffler  Donor-targeting   0.0686
# 3 raphael warnock Voter-targeting   0.100 
# 4 raphael warnock Donor-targeting   0.223 

# Various t-tests ==============================================================
## Within party x chamber, Trump-mentioning ads more toxic? --------------------
t.test(
  df %>%
    filter(chamber == "House" & party == "Republican" & word_trump == 1) %>%
    .$toxicity,
  df %>%
    filter(chamber == "House" & party == "Republican" & word_trump == 0) %>%
    .$toxicity
)

t.test(
  df %>%
    filter(chamber == "House" & party == "Democrat" & word_trump == 1) %>%
    .$toxicity,
  df %>%
    filter(chamber == "House" & party == "Democrat" & word_trump == 0) %>%
    .$toxicity
)

## Not significant
t.test(
  df %>%
    filter(chamber == "Senate" & party == "Republican" & word_trump == 1) %>%
    .$toxicity,
  df %>%
    filter(chamber == "Senate" & party == "Republican" & word_trump == 0) %>%
    .$toxicity
)

t.test(
  df %>%
    filter(chamber == "Senate" & party == "Democrat" & word_trump == 1) %>%
    .$toxicity,
  df %>%
    filter(chamber == "Senate" & party == "Democrat" & word_trump == 0) %>%
    .$toxicity
)
