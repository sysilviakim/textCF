source(here::here("R", "utilities.R"))

# Load data ====================================================================
fb_house <- read_rds("data/tidy/fb_perspective_House.RDS")
fb_senate <- read_rds("data/tidy/fb_perspective_Senate.RDS")

# Substitute with updated classification =======================================
fb_house <- fb_house %>% donate_classify()
fb_senate <- fb_senate %>% donate_classify()

# Wrangle summary dataset ======================================================
df <- bind_rows(
  fb_house %>% mutate(chamber = "House"),
  fb_senate %>% mutate(chamber = "Senate")
) %>%
  rowwise() %>%
  mutate(
    party = simple_cap(tolower(party)),
    financial = simple_cap(tolower(financial))
  ) %>%
  ungroup()

summ_df <- summ_df_fxn(df %>% group_by(party, financial, chamber))
summ_df_full <- summ_df_fxn(
  df %>%
    filter(financial == "Donor-targeting") %>%
    mutate(
      type = case_when(
        type %in% c("WinRed", "ActBlue") ~ type,
        type != "WinRed" & party == "Republican" ~ "Other Rep.\nPlatform",
        type != "ActBlue" & party == "Democrat" ~ "Other Dem.\nPlatform"
      )
    ) %>%
    filter(!is.na(type)) %>%
    group_by(chamber, type),
  full = TRUE
)

# T-tests ======================================================================
## An example for when we'd need them in the main text
df %>%
  filter(chamber == "House" & party == "Democrat") %>%
  t.test(TOXICITY ~ financial, data = .)

# Level of Language OOO by Facebook Ad Type ====================================
p1 <- fb_perspective_plot(
  summ_df,
  xvar = "Toxic", se = "se_toxic",
  xlab = paste0(
    "Average Probability that Ad Text Would Be\n",
    "Perceived as Rude/Disrespectful/Toxic"
  )
)

pdf(here("fig", "fb_toxic_means_by_type_chamber.pdf"), width = 6, height = 2.8)
print(p1)
dev.off()

p2 <- fb_perspective_plot(
  summ_df,
  xvar = "Obscene", se = "se_obscene",
  xlab = paste0(
    "Average Probability that Ad Text Would Be\n",
    "Perceived as Obscene"
  )
)

pdf(here("fig", "fb_obscene_means_by_type_chamber.pdf"), width = 6, height = 2.8)
print(p2)
dev.off()

p3 <- fb_perspective_plot(
  summ_df,
  xvar = "Identity Attack", se = "se_identity",
  xlab = paste0(
    "Average Probability that Ad Text Would Be\n",
    "Perceived as an Identity Attack"
  )
)

pdf(here("fig", "fb_identity_means_by_type_chamber.pdf"), width = 6, height = 2.8)
print(p3)
dev.off()

# Subtypes within financial ads ================================================
p4 <- fb_perspective_plot(
  summ_df_full,
  full = TRUE,
  xvar = "Toxic", se = "se_toxic",
  xlab = paste0(
    "Average Probability that Ad Text Would Be\n",
    "Perceived as Rude/Disrespectful/Toxic"
  )
)
pdf(here("fig", "fb_toxic_financial_subtypes.pdf"), width = 6, height = 2.8)
print(p4)
dev.off()

p5 <- fb_perspective_plot(
  summ_df_full,
  full = TRUE,
  xvar = "Obscene", se = "se_obscene",
  xlab = paste0(
    "Average Probability that Ad Text Would Be\n",
    "Perceived as Obscene"
  )
)
pdf(here("fig", "fb_obscene_financial_subtypes.pdf"), width = 6, height = 2.8)
print(p5)
dev.off()

p6 <- fb_perspective_plot(
  summ_df_full,
  full = TRUE,
  xvar = "Identity Attack", se = "se_identity",
  xlab = paste0(
    "Average Probability that Ad Text Would Be\n",
    "Perceived as an Identity Attack"
  )
)
pdf(here("fig", "fb_identity_financial_subtypes.pdf"), width = 6, height = 2.8)
print(p6)
dev.off()

# By party =====================================================================
candidate_Tox <- fb_house %>%
  group_by(candidate, party) %>%
  dplyr::summarise(
    `Avg. prob. toxic` = mean(TOXICITY),
    N = n()
  )

toxDEMcand <- candidate_Tox %>%
  filter(party == "DEMOCRAT") %>%
  filter(N >= 20) %>%
  ggplot(aes(x = `Avg. prob. toxic`)) +
  geom_histogram() +
  labs(
    y = "Candidates",
    x = "Average usage of toxic language",
    title = "Democratic Candidates"
  ) +
  geom_vline(
    aes(xintercept = mean(`Avg. prob. toxic`)),
    col = "blue", size = 1.5
  ) +
  geom_text(
    aes(
      x = mean(`Avg. prob. toxic`) + .03,
      y = 25,
      label = paste0("Mean:", mean(`Avg. prob. toxic`) %>% round(3))
    )
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

toxREPcand <- candidate_Tox %>%
  filter(party == "REPUBLICAN") %>%
  filter(N >= 20) %>%
  ggplot(aes(x = `Avg. prob. toxic`)) +
  geom_histogram() +
  labs(
    y = "Candidates",
    x = "Average usage of toxic language",
    title = "Republican Candidates"
  ) +
  geom_vline(
    aes(xintercept = mean(`Avg. prob. toxic`)),
    col = "red", size = 1.5
  ) +
  geom_text(
    aes(
      x = mean(`Avg. prob. toxic`) + .05,
      y = 25,
      label = paste0("Mean:", mean(`Avg. prob. toxic`) %>% round(3))
    )
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylim(c(0, 30))

ggarrange(toxDEMcand, toxREPcand) %>%
  annotate_figure(
    top = text_grob(
      "Distribution of candidate-level average ad toxicity (US House)",
      face = "bold", size = 14
    )
  )
ggsave("fig/fig2_house_candidate_averages.pdf", width = 8, height = 5)

candidate_Tox %>%
  filter(N >= 20) %>%
  filter(party %in% c("DEMOCRAT", "REPUBLICAN")) %>%
  ggplot(
    aes(
      y = party,
      x = `Avg. prob. toxic`,
      color = party,
      label = candidate
    )
  ) +
  geom_boxplot() +
  geom_jitter(height = .25) +
  scale_color_brewer(palette = "Set1", direction = -1) +
  labs(y = "")
ggsave("fig/fig2B_house_candidate_averages.pdf", width = 8, height = 4)

# Keeping a type if N>100 for any party group:
keepPostType <- fb_house %>%
  dplyr::count(party, type) %>%
  filter(n >= 100, type != "") %>%
  mutate(post = glue("{type} posted by {party}")) %>%
  pull(post)

fb_house %>%
  # filter(type %in% c("ActBlue","Winred","Voter-targeting",
  #                    "Personal Contribution Link")) %>%
  mutate(post = glue("{type} posted by {party}")) %>%
  filter(post %in% keepPostType) %>%
  ggplot(aes(
    y = post,
    x = TOXICITY,
    fill = post
  )) +
  ggridges::geom_density_ridges() +
  xlim(c(0, .3)) +
  theme(legend.position = "none") +
  labs(
    y = "",
    x = "Prob. text would be perceived as rude/disrespectful/toxic",
    title = "US House",
    subtitle = "Type shown if at least 100 posts of a given type exists"
  )
ggsave("fig/ridges_by_type.pdf", width = 8, height = 6)

# Example of Toxic Ads =========================================================

## Dem
fb_house %>%
  filter(TOXICITY >= .75) %>%
  filter(party == "DEMOCRAT") %>%
  pull(ad_creative_body)

## Rep
fb_house %>%
  filter(TOXICITY >= .75) %>%
  filter(party == "REPUBLICAN") %>%
  pull(ad_creative_body)

fb_house %>%
  ggplot(aes(x = TOXICITY)) +
  geom_histogram() +
  geom_vline(aes(xintercept = mean(TOXICITY)), col = "red", size = 1.5)

tox_dem <- fb_house %>%
  filter(party == "DEMOCRAT") %>%
  ggplot(aes(x = TOXICITY)) +
  geom_histogram() +
  labs(
    x = "", y = "Number of ads",
    title = "Democrats"
  ) +
  geom_vline(aes(xintercept = mean(TOXICITY)), col = "blue", size = 1.5) +
  geom_text(
    aes(
      x = mean(TOXICITY) + .2,
      y = 1000,
      label = paste0("Mean:", mean(TOXICITY) %>% round(2))
    )
  )

tox_rep <- fb_house %>%
  filter(party == "REPUBLICAN") %>%
  ggplot(aes(x = TOXICITY)) +
  geom_histogram() +
  labs(
    x = "", y = "Number of ads",
    title = "Republicans"
  ) +
  geom_vline(aes(xintercept = mean(TOXICITY)), col = "red", size = 1.5) +
  geom_text(
    aes(
      x = mean(TOXICITY) + .15,
      y = 1000,
      label = paste0("Mean:", mean(TOXICITY) %>% round(2))
    )
  )

ggarrange(tox_rep, tox_dem) %>%
  annotate_figure(
    top = text_grob("Distribution of toxicity probabilities in Facebook ads",
      face = "bold", size = 14
    ),
    bottom = text_grob("Prob(Ad text is toxic)")
  )
ggsave("fig/SI_ad_level_toxicity.pdf", width = 8, height = 5)

# Initial analysis =============================================================
fb_house %>%
  group_by(party) %>%
  dplyr::summarise(
    `Toxic` = mean(TOXICITY),
    `Identity attack` = mean(IDENTITY_ATTACK),
    `Obscene` = mean(IDENTITY_ATTACK),
    `Number of ads` = n()
  )

fb_house %>%
  group_by(type) %>%
  dplyr::summarise(
    `Toxic` = mean(TOXICITY),
    `Identity attack` = mean(IDENTITY_ATTACK),
    `Obscene` = mean(IDENTITY_ATTACK),
    `Number of ads` = n()
  ) %>%
  dplyr::arrange(-Toxic) %>%
  filter(type != "")

fb_house %>%
  group_by(party, type) %>%
  dplyr::summarise(
    `Toxic` = mean(TOXICITY),
    # `Identity attack` = mean(IDENTITY_ATTACK),
    `Obscene` = mean(IDENTITY_ATTACK),
    N = n()
  ) %>%
  filter(N >= 100, type != "") %>%
  mutate(post = glue("{type} posted by {party}")) %>%
  pivot_longer(cols = c(Toxic, `Obscene`)) %>%
  ggplot(
    aes(
      y = post,
      x = value,
      color = name
    )
  ) +
  geom_point() +
  facet_grid(~name)

temp0notext <- candidate_Tox %>%
  filter(N >= 10) %>%
  filter(party %in% c("DEMOCRAT", "REPUBLICAN")) %>%
  ggplot(aes(
    y = party,
    x = `Avg. prob. toxic`,
    color = party,
    label = candidate
  )) +
  geom_jitter() +
  scale_color_brewer(palette = "Set1", direction = -1) +
  labs(y = "")

temp1 <- candidate_Tox %>%
  filter(N >= 10) %>%
  filter(party %in% c("DEMOCRAT", "REPUBLICAN")) %>%
  ggplot(
    aes(
      y = party,
      x = `Avg. prob. toxic`,
      color = party,
      label = candidate
    )
  ) +
  geom_jitter() +
  ggrepel::geom_text_repel(max.overlaps = 100) +
  scale_color_brewer(palette = "Set1", direction = -1) +
  labs(y = "")
ggarrange(temp0notext, temp1, ncol = 1)

fb_house %>%
  group_by(candidate, party) %>%
  dplyr::summarise(
    `Avg. prob. toxic` = mean(TOXICITY),
    N = n()
  ) %>%
  dplyr::arrange(-`Avg. prob. toxic`) %>%
  filter(str_detect(candidate, "GREEN"))
