source(here::here("R", "utilities.R"))

# Load data ====================================================================
load(here("data", "tidy", "fb_matched.Rda"))
load(here("data", "tidy", "fb_unique.Rda"))
fb_matched <- fb_matched %>%
  map(
    ~ .x %>%
      select(
        candidate, fb_ad_library_id, page_name, party, inc, state_po, pvi, 
        gender, proportion_female, contains("state_cd"), 
        ad_creative_body, ad_creative_link_caption, vote_share
      ) %>%
      mutate(ad_creative_body = trimws(gsub("\\s+", " ", ad_creative_body))) %>%
      filter(ad_creative_body != "") %>%
      filter(ad_creative_body != "{{product.brand}}") %>%
      filter(!(page_name == "Antonio Delgado" & party == "REPUBLICAN")) %>%
      mutate(ad_creative_link_caption = tolower(ad_creative_link_caption)) %>%
      rowwise() %>%
      simplify_ad_body() %>%
      simplify_ad_body() %>%
      mutate(
        type = case_when(
          grepl("actblue.com", ad_creative_link_caption) ~ "ActBlue",
          grepl("winred.com", ad_creative_link_caption) ~ "WinRed",
          grepl("ngpvan.com|myngp.com", ad_creative_link_caption) ~ "NGP VAN",
          grepl("anedot.com", ad_creative_link_caption) ~ "Anedot",
          grepl("victorypassport.com", ad_creative_link_caption) ~
            "Victory Passport",
          grepl("fundraiser", ad_creative_link_caption) ~ "Misc.",
          is.na(ad_creative_link_caption) ~ "Voter-targeting",
          grepl(
            "conversation with |town hall|meet |tour stop |iwillvote.com",
            ad_creative_link_caption
          ) ~ "Voter-targeting",
          grepl(".gov", ad_creative_link_caption) ~ "Government Information",
          grepl("secure.|act.|action.|go.", ad_creative_link_caption) ~
            "Personal Contribution Link",
          grepl("facebook.com|fb.me", ad_creative_link_caption) ~
            "Facebook Page",
        )
      ) %>%
      donate_classify() %>%
      ungroup()
  )
save(fb_matched, file = here("data", "tidy", "fb_matched_wrangled.Rda"))

load(
  max(
    list.files(
      here("output"),
      pattern = "persp_final_results_", full.names = TRUE
    )
  )
)

df <- df %>% select(candidate, ad_creative_body, chamber, toxicity, obscene)

# Merge data ===================================================================
df_unique <- bind_rows(
  left_join(fb_unique$senate, df %>% filter(chamber == "senate")) %>%
    mutate(chamber = "Senate"),
  left_join(fb_unique$house, df %>% filter(chamber == "house")) %>%
    mutate(chamber = "House")
) %>%
  rowwise() %>%
  mutate(
    party = simple_cap(tolower(party)),
    financial = simple_cap(tolower(financial))
  ) %>%
  ungroup()
assert_that(!any(is.na(df_unique$toxicity)))
save(df_unique, file = here("data", "tidy", "merged_unique.Rda"))

df_all <- bind_rows(
  left_join(fb_matched$senate, df %>% filter(chamber == "senate")) %>%
    mutate(chamber = "Senate"),
  left_join(fb_matched$house, df %>% filter(chamber == "house")) %>%
    mutate(chamber = "House")
) %>%
  rowwise() %>%
  mutate(
    party = simple_cap(tolower(party)),
    financial = simple_cap(tolower(financial))
  ) %>%
  ungroup()
assert_that(!any(is.na(df_all$toxicity)))
save(df_all, file = here("data", "tidy", "merged_all.Rda"))

# Wrangle summary dataset ======================================================
summ_unique <- summ_df_fxn(df_unique %>% group_by(party, financial, chamber))
summ_all <- summ_df_fxn(df_all %>% group_by(party, financial, chamber))

summ_donor_unique <- summ_df_fxn(
  df_unique %>%
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

summ_donor_all <- summ_df_fxn(
  df_all %>%
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
df_unique %>%
  filter(chamber == "House" & party == "Democrat") %>%
  t.test(toxicity ~ financial, data = .)

# Level of Toxicity by Facebook Ad Type ========================================
p1 <- fb_perspective_plot(
  summ_unique,
  xvar = "Toxic", se = "se_toxic",
  xlab = paste0(
    "Average Probability that Ad Text Would Be\n",
    "Perceived as Rude/Disrespectful/Toxic"
  )
)

pdf(
  here("fig", "fb_toxic_means_by_type_chamber.pdf"),
  width = 6, height = 2.8
)
print(p1)
dev.off()

p2 <- fb_perspective_plot(
  summ_all,
  xvar = "Toxic", se = "se_toxic",
  xlab = paste0(
    "Average Probability that Ad Text Would Be\n",
    "Perceived as Rude/Disrespectful/Toxic"
  )
)

pdf(
  here("fig", "fb_toxic_means_by_type_chamber_all.pdf"),
  width = 6, height = 2.8
)
print(p2)
dev.off()

# Subtypes within financial ads ================================================
# p3 <- fb_perspective_plot(
#   summ_donor_unique,
#   full = TRUE,
#   xvar = "Toxic", se = "se_toxic",
#   xlab = paste0(
#     "Average Probability that Ad Text Would Be\n",
#     "Perceived as Rude/Disrespectful/Toxic"
#   )
# )
# pdf(here("fig", "fb_toxic_financial_subtypes.pdf"), width = 6, height = 2.8)
# print(p3)
# dev.off()

# By party =====================================================================
candidate_Tox <- df %>%
  filter(chamber == "House") %>%
  group_by(candidate, party) %>%
  dplyr::summarise(
    `Avg. prob. toxic` = mean(toxicity),
    N = n()
  )

toxDEMcand <- candidate_Tox %>%
  filter(party == "Democrat") %>%
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
  filter(party == "Republican") %>%
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
ggsave(here("fig/fig2_house_candidate_averages.pdf"), width = 8, height = 5)

candidate_Tox %>%
  filter(N >= 20) %>%
  filter(party %in% c("Democrat", "Republican")) %>%
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
ggsave(here("fig/fig2B_house_candidate_averages.pdf"), width = 8, height = 4)

# Keeping a type if N>100 for any party group:
keepPostType <- fb_house %>%
  dplyr::count(party, type) %>%
  filter(n >= 100, type != "") %>%
  mutate(post = glue("{type} posted by {party}")) %>%
  pull(post)

fb_house %>%
  mutate(post = glue("{type} posted by {party}")) %>%
  filter(post %in% keepPostType) %>%
  ggplot(aes(y = post, x = toxicity, fill = post)) +
  ggridges::geom_density_ridges() +
  xlim(c(0, .3)) +
  theme(legend.position = "none") +
  labs(
    y = "",
    x = "Prob. text would be perceived as rude/disrespectful/toxic",
    title = "US House",
    subtitle = "Type shown if at least 100 posts of a given type exists"
  )
ggsave(here("fig/ridges_by_type.pdf"), width = 8, height = 6)

# Substantial interpretations ==================================================
percentage_calc("Donor", "House") ## 31.2
percentage_calc("Donor", "Senate") ## 15.7

percentage_calc("Voter", "House") ## 33.6
percentage_calc("Voter", "Senate") ## 28.8

percentage_calc(c = "House", p = "Democrat") ## 35.3
percentage_calc(c = "House", p = "Republican") ## 32.9

percentage_calc(c = "Senate", p = "Democrat") ## 29.4
percentage_calc(c = "Senate", p = "Republican") ## 16.2

# Example of Toxic Ads =========================================================
## Dem
fb_house %>%
  filter(toxicity >= .75) %>%
  filter(party == "Democrat") %>%
  pull(ad_creative_body)

## Rep
fb_house %>%
  filter(toxicity >= .75) %>%
  filter(party == "Republican") %>%
  pull(ad_creative_body)

fb_house %>%
  ggplot(aes(x = toxicity)) +
  geom_histogram() +
  geom_vline(aes(xintercept = mean(toxicity)), col = "red", size = 1.5)

tox_dem <- fb_house %>%
  filter(party == "Democrat") %>%
  ggplot(aes(x = toxicity)) +
  geom_histogram() +
  labs(
    x = "", y = "Number of ads",
    title = "Democrats"
  ) +
  geom_vline(aes(xintercept = mean(toxicity)), col = "blue", size = 1.5) +
  geom_text(
    aes(
      x = mean(toxicity) + .2,
      y = 1000,
      label = paste0("Mean:", mean(toxicity) %>% round(2))
    )
  )

tox_rep <- fb_house %>%
  filter(party == "Republican") %>%
  ggplot(aes(x = toxicity)) +
  geom_histogram() +
  labs(
    x = "", y = "Number of ads",
    title = "Republicans"
  ) +
  geom_vline(aes(xintercept = mean(toxicity)), col = "red", size = 1.5) +
  geom_text(
    aes(
      x = mean(toxicity) + .15,
      y = 1000,
      label = paste0("Mean:", mean(toxicity) %>% round(2))
    )
  )

ggarrange(tox_rep, tox_dem) %>%
  annotate_figure(
    top = text_grob("Distribution of toxicity probabilities in Facebook ads",
      face = "bold", size = 14
    ),
    bottom = text_grob("Prob(Ad text is toxic)")
  )
ggsave(here("fig/SI_ad_level_toxicity.pdf"), width = 8, height = 5)

# Initial analysis =============================================================
fb_house %>%
  group_by(party) %>%
  dplyr::summarise(`Toxic` = mean(toxicity), `Number of ads` = n())

fb_house %>%
  group_by(type) %>%
  dplyr::summarise(
    `Toxic` = mean(toxicity),
    `Number of ads` = n()
  ) %>%
  dplyr::arrange(-Toxic) %>%
  filter(type != "")

fb_house %>%
  group_by(party, type) %>%
  dplyr::summarise(
    `Toxic` = mean(toxicity), `Obscene` = mean(obscene), N = n()
  ) %>%
  filter(N >= 100, type != "") %>%
  mutate(post = glue("{type} posted by {party}")) %>%
  pivot_longer(cols = c(Toxic, `Obscene`)) %>%
  ggplot(aes(y = post, x = value, color = name)) +
  geom_point() +
  facet_grid(~name)

temp0notext <- candidate_Tox %>%
  filter(N >= 10) %>%
  filter(party %in% c("Democrat", "Republican")) %>%
  ggplot(
    aes(
      y = party,
      x = `Avg. prob. toxic`,
      color = party,
      label = candidate
    )
  ) +
  geom_jitter() +
  scale_color_brewer(palette = "Set1", direction = -1) +
  labs(y = "")

temp1 <- candidate_Tox %>%
  filter(N >= 10) %>%
  filter(party %in% c("Democrat", "Republican")) %>%
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
    `Avg. prob. toxic` = mean(toxicity),
    N = n()
  ) %>%
  dplyr::arrange(-`Avg. prob. toxic`) %>%
  filter(str_detect(candidate, "GREEN"))
