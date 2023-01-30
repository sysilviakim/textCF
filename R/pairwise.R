source(here::here("R", "utilities.R"))
df <- read_rds(here("data", "tidy", "toxicity.RDS")) %>%
  select(
    candidate, chamber, ad_creative_body, toxicity, party, inc, financial, 
    everything()
  )
summary(df$toxicity)

# Create combinations ==========================================================
set.seed(12345)
x1 <- sample(seq(nrow(df)), 4000)
x2 <- sample(seq(nrow(df)), 4000)
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

pair_noanswer <- pair_df %>% select(-toxic1, -toxic2) %>% mutate(choice = "")
write_excel_csv(pair_noanswer, file = here("output", "pair_noanswer.csv"))

# Import RA human answers ======================================================


