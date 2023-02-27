source(here::here("R", "utilities.R"))
library(fixest)

# Load data ====================================================================
load(here("data", "tidy", "merged_unique.Rda"))
df_unique <- df_unique %>%
  clean_names() %>%
  clean_candidate() %>%
  filter(candidate != "michael san nicolas") %>%
  dedup() %>%
  mutate(
    mean_ad_delivery_start_time =
      as.Date(round(mean_ad_delivery_start_time), origin = "1970-01-01"),
    mean_ad_delivery_stop_time =
      as.Date(round(mean_ad_delivery_stop_time), origin = "1970-01-01"),
    min_ad_delivery_start_time =
      as.Date(round(min_ad_delivery_start_time), origin = "1970-01-01"),
    min_ad_delivery_stop_time =
      as.Date(round(min_ad_delivery_stop_time), origin = "1970-01-01"),
    max_ad_delivery_start_time =
      as.Date(round(max_ad_delivery_start_time), origin = "1970-01-01"),
    max_ad_delivery_stop_time =
      as.Date(round(max_ad_delivery_stop_time), origin = "1970-01-01"),
    safety = case_when(
      party == "Democrat" ~ pvi,
      party == "Republican" ~ -pvi
    )
  ) %>%
  filter(!(is.na(party) | party == "NANA" | party == "Independent"))

## Only non-running Senators
assert_that(
  df_unique %>%
    filter(
      is.na(state_po) & chamber != "senate" & candidate != "tracy jennings"
    ) %>%
    nrow() == 0
)
assert_that(
  df_unique %>%
    filter(
      is.na(party) & chamber != "senate" & candidate != "tracy jennings"
    ) %>%
    nrow() == 0
)

temp <- df_unique %>%
  filter(!is.na(party) & !is.na(state_po) & party != "INDEPENDENT") %>%
  party_factor(., outvar = "Type") %>%
  mutate(
    financial = factor(
      financial,
      levels = c("Voter-targeting", "Donor-targeting")
    )
  ) %>%
  select(financial, everything())

write_rds(temp, file = here("data", "tidy", "toxicity.RDS"))

## Assertions for sanity check
assert_that(!any(is.na(temp$inc)))
assert_that(!any(is.na(temp$pvi)))
assert_that(!any(is.na(temp$chamber)))
assert_that(!any(is.na(temp$state_po)))
assert_that(!any(is.na(temp$party)))
nrow(temp)

# Over time plot ===============================================================
## (Ended up not using)
color4_modified <- color4
names(color4_modified) <- gsub("\n", " ", names(color4_modified))
p <- temp %>%
  mutate(year_month = floor_date(min_ad_delivery_start_time, "month")) %>%
  filter(year_month >= as.Date("2019-01-01")) %>%
  mutate(Type = gsub("\n", " ", Type)) %>%
  group_by(Type, year_month) %>%
  count() %>%
  ggplot(aes(x = year_month, y = n, group = Type, color = Type)) +
  geom_line() +
  scale_color_manual(values = color4_modified) +
  xlab("Election Cycle, Monthly") +
  ylab("Number of Unique Ads") +
  scale_x_date(breaks = "3 months", date_labels = "%Y\n%b") +
  scale_y_continuous(labels = scales::comma)

pdf(here("fig", "n_unique_ads_over_time.pdf"), width = 5, height = 3.5)
print(
  plot_notitle(pdf_default(p)) + theme(legend.position = "bottom") +
    guides(colour = guide_legend(nrow = 2))
)
dev.off()

# OLS first for reference (simple model) =======================================
## Clustered model
fit <- lm(
  toxicity ~
    party * financial + chamber + inc + safety + gender +
    min_ad_delivery_start_time + state_po,
  temp
)
summary(fit)

fit_se_cluster <- feols(
  toxicity ~
    party * financial + chamber + inc + safety + gender +
    min_ad_delivery_start_time + state_po,
  temp
)

summary(fit_se_cluster, cluster = ~candidate)
etable(fit_se_cluster, cluster = "candidate", replace = TRUE)
etable(
  fit_se_cluster,
  cluster = "candidate", tex = TRUE, replace = TRUE,
  file = here("tab", "fit_cand_cluster_toxicity.tex")
)

## One without interaction term as requested by reviewer
fit <- lm(
  toxicity ~
    party + financial + chamber + inc + safety + gender +
    min_ad_delivery_start_time + state_po,
  temp
)
summary(fit)

fit_se_cluster <- feols(
  toxicity ~
    party + financial + chamber + inc + safety + gender +
    min_ad_delivery_start_time + state_po,
  temp
)

summary(fit_se_cluster, cluster = ~candidate)
etable(fit_se_cluster, cluster = "candidate", replace = TRUE)
etable(
  fit_se_cluster,
  cluster = "candidate", tex = TRUE, replace = TRUE,
  file = here("tab", "fit_cand_cluster_toxicity_no_interaction.tex")
)

fit_fe <- feols(
  toxicity ~
    financial + min_ad_delivery_start_time | candidate,
  temp
)

summary(fit_fe)
etable(fit_fe, replace = TRUE)
etable(
  fit_fe,
  tex = TRUE, replace = TRUE,
  file = here("tab", "fit_fe_toxicity.tex")
)

# Toxicity of Trump-mentioning ads across chambers =============================
temp <- temp %>%
  mutate(
    Trump = case_when(
      word_trump == 0 & party == "Democrat" ~ "Dem.,\nNo",
      word_trump == 1 & party == "Democrat" ~ "Dem.,\nYes",
      word_trump == 0 & party == "Republican" ~ "Rep.,\nNo",
      word_trump == 1 & party == "Republican" ~ "Rep.,\nYes"
    ),
    Trump = factor(
      Trump,
      levels = c(
        "Dem.,\nNo", "Dem.,\nYes",
        "Rep.,\nNo", "Rep.,\nYes"
      )
    )
  )

p <- ggplot(temp) +
  aes(x = Trump, y = toxicity, group = Trump, fill = Trump, color = NULL) +
  ## geom_boxplot(fill = "#0c4c8a") +
  geom_boxplot() +
  coord_cartesian(ylim = c(0, 1)) +
  labs(x = "Party and Trump Mentions", y = "Toxicity") +
  facet_wrap(~chamber) +
  scale_fill_manual(
    values = c(
      `Dem.,\nNo` = "#67a9cf",
      `Dem.,\nYes` = "#2166ac",
      `Rep.,\nNo` = "#ef8a62",
      `Rep.,\nYes` = "#b2182b"
    )
  )

plot_nolegend(pdf_default(p))
ggsave(
  here("fig", "toxicity_by_party_trump_chamber.pdf"),
  width = 6, height = 4
)
