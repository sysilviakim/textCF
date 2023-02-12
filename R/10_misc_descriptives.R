source(here::here("R", "utilities.R"))

# Import; will deal with descriptives ==========================================
df <- read_rds(here("data", "tidy", "toxicity.RDS")) %>%
  select(
    candidate, chamber, ad_creative_body, toxicity, party, inc, financial,
    everything()
  )
summary(df$toxicity)

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

# Create combinations ==========================================================
set.seed(12345)
x1 <- sample(seq(nrow(df)), 5000)
x2 <- sample(seq(nrow(df)), 5000)
idx <- which(abs(df$toxicity[x1] - df$toxicity[x2]) > 0.1)

pair_indices <- tibble(i1 = x1[idx], i2 = x2[idx]) %>%
  filter(i1 != i2)
assert_that(nrow(pair_indices) > 1000)
pair_indices <- pair_indices %>%
  sample_n(1000)

# Create answer keys and export without toxicity answers =======================
pair_df <- pair_indices %>%
  rowwise() %>%
  mutate(
    body1 = df$ad_creative_body[i1],
    body2 = df$ad_creative_body[i2],
    toxic1 = df$toxicity[i1],
    toxic2 = df$toxicity[i2],
    answer = case_when(
      df$toxicity[i1] > df$toxicity[i2] ~ 1,
      TRUE ~ 2
    ),
    answer = factor(answer, levels = c(1, 2))
  )

pair_noanswer <- pair_df %>%
  select(-toxic1, -toxic2) %>%
  mutate(choice = "")
write_excel_csv(pair_noanswer, file = here("output", "pair_noanswer.csv"))

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
