source(here::here("R", "utilities.R"))
library(keyATM)

# Load data ====================================================================
load(here("data", "tidy", "merged_unique.Rda"))

## Corpus creation
fb_corpus <- corpus(
  df_unique$ad_creative_body,
  docvars = tibble(
    financial = df_unique$financial,
    candidate = df_unique$candidate,
    party = df_unique$party,
    vote_share = df_unique$vote_share
  )
)

# Tokenize documents ===========================================================
toks_FB_STEMMED <- tokens(fb_corpus) %>%
  tokens(
    remove_url = TRUE,
    remove_punct = TRUE,
    include_docvars = TRUE
  ) %>%
  tokens_tolower() %>%
  tokens_remove(stopwords("english")) %>%
  tokens_remove(stopwords("spanish")) %>%
  # tokens_remove(c("rt", "amp", "u8")) %>%
  tokens_remove(letters) %>%
  tokens_compound(
    pattern = phrase(c(
      "stock market",
      "white house",
      "right to choose",
      "right to life",
      "climate change",
      "green new deal",
      "gdp growth",
      "dow jones",
      "wall street",
      "income inequal*",
      "gay marriage",
      "fossil fuel",
      "insurance compan*",
      "drug compan*",
      "prescription drug*",
      "pharmaceutical compan*",
      "big pharma",
      "public health",
      "health care",
      "drug abuse",
      "drug pric*",
      "gas pric*",
      "national security",
      "affordable care act",
      "middle class",
      "student debt",
      "national debt"
    ))
  ) %>%
  tokens_wordstem()

dfm_FB_ad_stemmed <- dfm(toks_FB_STEMMED)

# Stemmed keywords =============================================================
keywords_stemmed <- list(
  Economy = c(
    "work",
    "wage",
    "unemploy",
    "gdp", "manufactur", "trade",
    "usmca", "#usmca",
    "import", "export", "tariff",
    "econom",
    "budget",
    "tax",
    "fiscal",
    "expenditur",
    "#trumpbudget",
    "trumptax", "#goptaxbil", "#taxreform",
    "employ",
    "job",
    "paycheck",
    "jobless", "incom",
    "incom_inequ",
    "inequ",
    "poverti",
    "middle-class"
  ),
  Health = c(
    "health_care", "health",
    "afford_care_act",
    "#affordablecareact",
    "hospit", "hospital",
    "#obamacar", "obamacar",
    "influenza",
    "#mentalhealth",
    "overdos", "opioid", "#opioidcrisi",
    "pandem", "#covid19pandem",
    "public_health",
    "covid", "#covid", "covid-19", "#covid19",
    "coronavirus", "virus",
    "infect", "infecti", "disinfect",
    "vaccin", "#getvaccin", "#covid19vaccin",
    "moderna", "pfizer", "astrazeneca"
  ),
  # Safety_and_Crime = c(
  #   "crime", "drug", "crimin", "gunshot",
  #   "murder", "robber", "vandal"
  # ),
  Social_Issues_A = c(
    "rifl", "nra", "gun(s)", "violenc",
    "shoot", "shooter"
  ),
  Social_Issues_B = c(
    "abort", "pro-choic", "reproduct",
    "pregnanc", "pregnant",
    "naral", "parenthood",
    "sexism", "#metoo",
    "gay", "lesbian", "lbgtq"
  ),
  # Race = c(
  #   "anti-discrimin",
  #   "discrimin", "discriminatori",
  #   "racism", "racial", "anti-racist",
  #   "bias",
  #   "#criminaljusticereform",
  #   "racialprofil",
  #   "floyd", "#georgefloyd"
  # ),
  Immigration = c(
    "immigr",
    "border",
    "migrant", "migrat",
    "dreamer", "daca", "#daca"
  ),
  Environment = c(
    "earth", "climat", "weather", "oil", "drill",
    "environ", "environment",
    "fossil_fuel",
    "green_new_deal",
    "temperatur"
  ),
  President = c(
    "trump", "presid",
    "@realdonaldtrump",
    "trump-republican"
  ),
  Patriotic = c("america", "usa", "nation", "country", "patriot", "#teamusa"),
  # Foreign_Affairs = c(
  #   "foreign", "global",
  #   "china", "russia", "iran", "war", "afghanistan",
  #   "macron", "merkel"
  # ),
  Education = c(
    "educ", "school", "academi", "univers", "tuition",
    "student",
    "student_debt"
  )
)

# Necessary to drop non-textual matrix rows:
dfmAnalaysis <- dfm_subset(dfm_FB_ad_stemmed, ntoken(dfm_FB_ad_stemmed) > 0)

# # Prep keyATM object:
key_dfm <- keyATM_read(
  texts = dfmAnalaysis,
  keep_docnames = TRUE
) # Will merge based on docnames later!

# Base model ===================================================================
out_stemmed <- keyATM(
  docs = key_dfm,
  no_keyword_topics = 5,
  keywords = keywords_stemmed,
  model = "base",
  options = list(seed = 81477)
)

# Extract top topics
TOPICS_base <- top_topics(out_stemmed)

# Add docnames to the output:
TOPICS_base$docname <- rownames(out_stemmed$theta)

# And/or used this object for merging:
exportObj <- out_stemmed$theta %>%
  as_tibble(rownames = "doc_id") %>%
  pivot_longer(cols = !contains("doc_id")) %>%
  group_by(doc_id) %>%
  slice_max(value) %>%
  ungroup() %>%
  rename(Rank1 = name)

# Show which words are most prognostic of topics:
top_words(out_stemmed)

# Old method: Check the objects for merging are of the same length
# assertthat::are_equal(nrow(dfmAnalaysis), nrow(TOPICS_base))

# Prepare a tibble based on the DFM attributes:
tibbleForAnalysis <- docvars(dfmAnalaysis) %>%
  tibble() %>%
  mutate(doc_id = docnames(dfmAnalaysis))

merged <- left_join(tibbleForAnalysis, exportObj,
  by = "doc_id"
)

merged <- merged %>%
  mutate(
    # Coarsened groups:
    topic = case_when(
      Rank1 == "1_Economy" ~ "Economy and jobs",
      Rank1 == "2_Health" ~ "Health care or public health",
      Rank1 == "8_Patriotic" ~ "Patriotic speech",
      Rank1 == "5_Immigration" ~ "Immigration",
      Rank1 == "7_President" ~ "Trump",
      !is.na(Rank1) ~ "Various other / mixed"
    ),
    # Different bunching of categories:
    topic_Redefined = case_when(
      Rank1 == "1_Economy" ~ "Economy and jobs",
      Rank1 == "2_Health" ~ "Health care or public health",
      Rank1 == "8_Patriotic" ~ "Patriotic speech",
      Rank1 == "5_Immigration" ~ "Immigration",
      Rank1 == "7_President" ~ "Trump",
      Rank1 == "3_Social_Issues_A" |
        Rank1 == "4_Social_Issues_B" |
        Rank1 == "6_Environment" |
        Rank1 == "9_Education" ~ "Other (envir., educ. & social issues)",
      !is.na(Rank1) ~ "Generic campaigning: esp. $ solicitations"
    ),
    topRank = case_when(
      Rank1 == "1_Economy" ~ "Economy and jobs",
      Rank1 == "2_Health" ~ "Health care or public health",
      Rank1 == "3_Social_Issues_A" ~ "Social issues (guns)",
      Rank1 == "4_Social_Issues_B" ~ "Social issues (identity groups)",
      Rank1 == "5_Immigration" ~ "Immigration",
      Rank1 == "6_Environment" ~ "Environmental issues",
      Rank1 == "7_President" ~ "Trump",
      Rank1 == "8_Patriotic" ~ "Patriotism, nation, etc.",
      Rank1 == "9_Education" ~ "Education-related issues",
      Rank1 == "Other_1" ~ "Misc. theme 1 (unlabelled)",
      Rank1 == "Other_2" ~ "Misc. theme 2 (unlabelled)",
      Rank1 == "Other_3" ~ "Misc. theme 3 (unlabelled)",
      Rank1 == "Other_4" ~ "Misc. theme 4 (unlabelled)",
      Rank1 == "Other_5" ~ "Misc. theme 5 (unlabelled)"
    )
  ) %>%
  party_factor(., outvar = "post") %>%
  filter(!is.na(party) & party != "NANA" & party != "Independent") %>%
  mutate(chamber = case_when(chamber == "senate" ~ "Senate", TRUE ~ "House"))
save(merged, file = here("output", "merged_keyatm.Rda"))

pdf(here("fig", "keyatm_by_party.pdf"), width = 6.2, height = 2.5)
topic_plot(
  merged %>%
    mutate(party = factor(party, levels = c("Republican", "Democrat"))),
  gvar = "party"
)
dev.off()

pdf(here("fig", "keyatm_by_party_type.pdf"), width = 6.2, height = 3.5)
topic_plot(merged, gvar = "post")
dev.off()

pdf(here("fig", "keyatm_by_party_type_chamber.pdf"), width = 6.2, height = 3.5)
topic_plot(
  merged %>%
    group_by(post, chamber) %>%
    dplyr::count(post, topic, chamber),
  gvar = "post", grouped = TRUE
) +
  facet_wrap(~chamber)
dev.off()

merged %>%
  group_by(party) %>%
  dplyr::count(party, topic_Redefined) %>%
  mutate(prop = n / sum(n)) %>%
  filter(party %in% c("DEMOCRAT", "REPUBLICAN")) %>%
  mutate(post = glue("Ads by {party}S")) %>%
  ggplot(aes(x = prop * 100, y = post, fill = fct_rev(topic_Redefined))) +
  geom_bar(stat = "identity", alpha = .7, width = .5) +
  scale_fill_brewer(type = "qual", palette = 3) +
  labs(
    y = "", x = "Percent of Ads", fill = "Topic",
    title = "Ad topics by party"
  ) +
  theme_bw()
ggsave("fig/keyATM1B.pdf", width = 8, height = 6)

merged %>%
  group_by(party, financial) %>%
  dplyr::count(party, financial, topic_Redefined) %>%
  mutate(prop = n / sum(n)) %>%
  filter(party %in% c("DEMOCRAT", "REPUBLICAN")) %>%
  mutate(post = glue("{financial} posted by {party}")) %>%
  ggplot(aes(x = prop * 100, y = post, fill = fct_rev(topic_Redefined))) +
  geom_bar(stat = "identity", alpha = .7, width = .5) +
  scale_fill_brewer(type = "qual", palette = 3) +
  labs(
    y = "", x = "Percent of Ads", fill = "Topic",
    title = "Ad topics by ad type and by party"
  ) +
  theme_bw()
ggsave("fig/keyATM2B.pdf", width = 8, height = 6)

merged %>%
  group_by(party, financial, chamber) %>%
  dplyr::count(party, financial, topic_Redefined, chamber) %>%
  mutate(prop = n / sum(n)) %>%
  filter(party %in% c("DEMOCRAT", "REPUBLICAN")) %>%
  mutate(post = glue("{financial} posted by {party}")) %>%
  ggplot(aes(x = prop * 100, y = post, fill = fct_rev(topic_Redefined))) +
  geom_bar(stat = "identity", alpha = .7, width = .4) +
  scale_fill_brewer(type = "qual", palette = 3) +
  facet_grid(~chamber) +
  labs(
    y = "", x = "Percent of Ads", fill = "Ad theme / topic",
    title = "Ad topics by party, chamber, and ad type"
  ) +
  theme_bw()
ggsave("fig/keyATM3B.pdf", width = 8, height = 6)

# SI material ==================================================================
merged %>% count(Rank1, topic_Redefined)

S1 <- merged %>%
  count(topRank) %>%
  ggplot(aes(x = n, y = fct_reorder(topRank, n))) +
  geom_point() +
  theme_bw() +
  labs(
    x = "Number of ads",
    y = "",
    subtitle = "Full list of topics"
  )

S2 <- merged %>%
  count(topic_Redefined) %>%
  ggplot(aes(x = n, y = fct_reorder(topic_Redefined, n))) +
  geom_col(width = .5) +
  theme_bw() +
  labs(
    x = "Number of ads",
    y = "",
    subtitle = "A simplified list of topics"
  )

ggpubr::ggarrange(S1, S2)
ggsave("fig/keyATM3_topics_summary2.pdf", width = 9, height = 5)

# SI table =====================================================================
values_fig(key_viz) %>%
  filter(Ranking <= 5) %>%
  mutate(`Pre-defined topic` = case_when(
    Topic == "1_Economy" ~ "Economy and jobs",
    Topic == "2_Health" ~ "Health care or public health",
    Topic == "3_Social_Issues_A" ~ "Social issues (guns)",
    Topic == "4_Social_Issues_B" ~ "Social issues (identity groups)",
    Topic == "5_Immigration" ~ "Immigration",
    Topic == "6_Environment" ~ "Environmental issues",
    Topic == "7_President" ~ "Trump",
    Topic == "8_Patriotic" ~ "Patriotism, nation, etc.",
    Topic == "9_Education" ~ "Education-related issues"
  )) %>%
  ungroup() %>%
  dplyr::select(-Ranking, -Topic) %>%
  relocate(
    `Pre-defined topic`,
    Word,
    `Proportion(%)`,
    WordCount
  ) %>%
  kable(
    format = "latex",
    booktabs = T
  )

# Background plots =============================================================
summary(key_dfm)

visualize_keywords(docs = key_dfm, keywords = keywords_stemmed[1:2])
visualize_keywords(docs = key_dfm, keywords = keywords_stemmed[3:5])
visualize_keywords(docs = key_dfm, keywords = keywords_stemmed[c(6, 9)])
visualize_keywords(docs = key_dfm, keywords = keywords_stemmed[c(3, 4, 5, 9)])
visualize_keywords(docs = key_dfm, keywords = keywords_stemmed[c(1, 7, 8)])

key_viz <- visualize_keywords(docs = key_dfm, keywords = keywords_stemmed)
key_viz
values_fig(key_viz) %>% View()

top_terms <- out_stemmed$phi %>%
  t() %>%
  as_tibble(rownames = "word") %>%
  pivot_longer(-word, names_to = "topic", values_to = "phi") %>%
  group_by(topic) %>%
  top_n(7, phi) %>%
  arrange(topic, desc(phi)) %>%
  group_by(topic) %>%
  summarise(top_words = paste(word, collapse = ", "), .groups = "drop")

top_terms

top_topics <- out_stemmed$theta %>%
  as_tibble(rownames = "ad") %>%
  pivot_longer(-ad, names_to = "topic", values_to = "theta") %>%
  group_by(topic) %>%
  summarise(mean_theta = mean(theta), .groups = "drop") %>%
  arrange(desc(mean_theta))

top_topics

top_topics %>%
  left_join(top_terms, by = "topic") %>%
  mutate(topic = reorder(topic, mean_theta)) %>%
  ggplot(aes(topic, mean_theta, label = top_words, fill = topic)) +
  geom_col(show.legend = FALSE) +
  geom_text(hjust = 0, nudge_y = 0.0005, size = 3) +
  coord_flip() +
  scale_y_continuous(
    expand = c(0, 0), limits = c(0, 0.35), labels = scales::percent
  ) +
  theme_bw() +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank()
  ) +
  labs(x = NULL, y = expression(theta))
