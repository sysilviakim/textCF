library(tidyverse)
library(keyATM)

# Load data ====================================================================
load(here("data", "tidy", "fb_unique.Rda"))
#load(here("output", "fb_quanteda.Rda"))

fb_corpus <- fb_unique %>%
  imap(
    ~ corpus(
      .x$ad_creative_body,
      docvars = tibble(
        financial = .x$financial,
        candidate = .x$candidate,
        party = .x$party,
        vote_share = .x$vote_share,
        chamber = .y
      )
    )
  )

## Rename the documents, adding by-chamber prefix:
docnames(fb_corpus$house) <- paste0("house_", 1:ndoc(fb_corpus$house))
docnames(fb_corpus$senate) <- paste0("senate_", 1:ndoc(fb_corpus$senate))

## Combine two chambers
corp_FB <- fb_corpus$senate + fb_corpus$house

# Tokenize documents ===========================================================
toks_FB_STEMMED <- tokens(corp_FB) %>%
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
  tokens_compound(pattern = phrase(c('stock market',
                                     'white house',
                                     'right to choose',
                                     'right to life',
                                     'climate change',
                                     'green new deal',
                                     'gdp growth',
                                     'dow jones',
                                     'wall street', 
                                     'income inequal*', 
                                     'gay marriage',
                                     'fossil fuel',
                                     'insurance compan*',
                                     'drug compan*',
                                     'prescription drug*',
                                     'pharmaceutical compan*',
                                     'big pharma',
                                     'public health',
                                     'health care',
                                     'drug abuse',
                                     'drug pric*',
                                     'gas pric*',
                                     'national security',
                                     'affordable care act',
                                     'middle class',
                                     'student debt',
                                     'national debt'))) %>%
  tokens_wordstem()

dfm_FB_ad_stemmed <- dfm(toks_FB_STEMMED)

#####################################################################
keywords_stemmed <- list(Economy = c(
  "work",
  "wage",
  "unemploy",
  "gdp","manufactur","trade",
  "usmca","#usmca",
  "import","export","tariff",
  "econom",  
    "budget",
  "tax",
   "fiscal",
   "expenditur",
    "#trumpbudget",
    "trumptax","#goptaxbil","#taxreform",
    "employ",
    "job",
    "paycheck",
    "jobless","incom",
    "incom_inequ",
    "inequ",
    "poverti",
    "middle-class"),
  Health = c("health_care","health",
               "afford_care_act",
               "#affordablecareact",
               "hospit","hospital",
               "#obamacar","obamacar",
               "influenza",
               "#mentalhealth",
               "overdos","opioid","#opioidcrisi",
               "pandem","#covid19pandem",
                     "public_health",
                     "covid","#covid","covid-19","#covid19",
                     "coronavirus","virus",
                     "infect","infecti","disinfect",
                     "vaccin","#getvaccin","#covid19vaccin",
                     "moderna","pfizer","astrazeneca"),
  # Safety_and_Crime = c("crime","drug","crimin","gunshot",
  #            "murder","robber","vandal"),
  Social_Issues_A = c("rifl", "nra","gun(s)","violenc",
                      "shoot","shooter"),
  Social_Issues_B = c("abort","pro-choic","reproduct",
                      "pregnanc","pregnant",
                      "naral","parenthood",
                      "sexism","#metoo",
                      "gay","lesbian","lbgtq"),
  # Race = c("anti-discrimin",
  #                     "discrimin","discriminatori",
  #                     "racism","racial","anti-racist",
  #                     "bias",
  #                     "#criminaljusticereform",
  #                     "racialprofil",
  #                     "floyd","#georgefloyd"),
  Immigration = c("immigr","immigr",
                  "border",
                  "migrant","migrat",
                  "dreamer","daca","#daca"),
  Environment = c("earth","climat","weather","oil","drill",
                  "environ","environment",
                  "fossil_fuel",
                  "green_new_deal",
                  "temperatur"),
  President = c("trump","presid",
                "@realdonaldtrump",
                "trump-republican"),
  Patriotic   = c("america", "usa", "nation","country","patriot","#teamusa"),
  # Foreign_Affairs = c("foreign","global",
  #                    "china","russia","iran","war","afghanistan",
  #                    "macron","merkel"),
  Education = c("educ","school","academi","univers","tuition",
                "student",
                "student_debt"))


# Necessary to drop non-textual matrix rows:
dfmAnalaysis <- dfm_subset(dfm_FB_ad_stemmed,ntoken(dfm_FB_ad_stemmed) > 0)

# # Prep keyATM object:
key_dfm <- keyATM_read(texts = dfmAnalaysis,
                             keep_docnames = TRUE) # Will merge based on docnames later!


# House only:
# dfmHouse <- dfm_FB_ad_stemmed %>%
#                 dfm_subset(chamber=="house")
# 
# # Necessary to drop non-textual matrix rows:
# dfmHouseAnalaysis <- dfm_subset(dfmHouse,ntoken(dfmHouse) > 0)
# 
# # Add IDs to the matrix
# dfmHouseAnalaysis$tempID <- 1:nrow(dfmHouseAnalaysis)  
# 
# # Prep keyATM object:
# key_dfm_House <- keyATM_read(texts = dfmHouseAnalaysis,
#                              keep_docnames = TRUE)

############
# BASE MODEL
############

out_stemmed <- keyATM(docs  = key_dfm,  
                            no_keyword_topics = 5,              
                            keywords          = keywords_stemmed,     
                            model             = "base",       
                            options           = list(seed = 81477))

# out_stemmed_house <- keyATM(docs  = key_dfm_House,  
#                       no_keyword_topics = 5,              
#                       keywords          = keywords_stemmed,     
#                       model             = "base",       
#                       options           = list(seed = 81477))

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
                    by ="doc_id")

merged %>% count(Rank1)

relab <- function(x) {
  ifelse(x=="1_Economy","Economy",
         ifelse(x=="2_Health","Health care & public health",x))
}

merged <- merged %>% 
  mutate(topic = case_when(
    Rank1 == "1_Economy"    ~ "Economy and jobs",
    Rank1 == "2_Health"    ~ "Health care or public health",
    Rank1 == "8_Patriotic" ~ "Patriotic speech",
    Rank1 == "5_Immigration"     ~ "Immigration",
    Rank1 == "7_President"     ~ "Trump",
    !is.na(Rank1) ~ "Various other / mixed"),
    topRank = case_when(
      Rank1 == "1_Economy"    ~ "Economy and jobs",
      Rank1 == "2_Health"    ~ "Health care or public health",
      Rank1 == "3_Social_Issues_A" ~ "Social issues (guns)",
      Rank1 == "4_Social_Issues_B" ~ "Social issues (identity groups)",
      Rank1 == "5_Immigration"     ~ "Immigration",
      Rank1 == "6_Environment"     ~ "Environmental issues",
      Rank1 == "7_President"     ~ "Trump",
      Rank1 == "8_Patriotic"     ~ "Patriotism, nation, etc.",
      Rank1 == "9_Education"     ~ "Education-related issues",
      Rank1 == "Other_1"     ~ "Misc. theme 1 (unlabelled)",
      Rank1 == "Other_2"     ~ "Misc. theme 2 (unlabelled)",
      Rank1 == "Other_3"     ~ "Misc. theme 3 (unlabelled)",
      Rank1 == "Other_4"     ~ "Misc. theme 4 (unlabelled)",
      Rank1 == "Other_5"     ~ "Misc. theme 5 (unlabelled)")
  )

merged %>% count(topRank) %>%
  ggplot(aes(x=n, y=fct_reorder(topRank,n))) +
  geom_point() +
  theme_bw()

# merged %>% 
#    filter(party %in% c("DEMOCRAT","REPUBLICAN")) %>%
#    group_by(party) %>%
#    count(topic) %>%
#     filter(topic %in% c("Trump","Patriotic speech","Immigration")) %>%
#    ggplot(aes(x=n,y=party,fill=topic)) + 
#    geom_bar(stat="identity",alpha=.7) +
#    scale_fill_brewer(type = "qual",palette = 3) +
#    labs(y="Number of posts", x ="", fill = "Topic")
# # scale_fill_viridis_d() +

merged %>%
  group_by(party) %>%
  dplyr::count(party, topic) %>%
  mutate(prop = n / sum(n)) %>%
  filter(party %in% c("DEMOCRAT","REPUBLICAN")) %>%
  mutate(post = glue("Ads by {party}S")) %>%
  ggplot(aes(x=prop*100,y=post,fill=fct_rev(topic))) + 
  geom_bar(stat="identity",alpha=.7, width = .5) +
  scale_fill_brewer(type = "qual",palette = 3) +
  labs(y="", x ="Percent of ads", fill = "Topic") +
  theme_bw()

merged %>%
  group_by(party, financial) %>%
  dplyr::count(party, financial, topic) %>%
  mutate(prop = n / sum(n)) %>%
  filter(party %in% c("DEMOCRAT","REPUBLICAN")) %>%
  mutate(post = glue("{financial} posted by {party}")) %>%
  ggplot(aes(x=prop*100,y=post,fill=fct_rev(topic))) + 
  geom_bar(stat="identity",alpha=.7, width = .5) +
  scale_fill_brewer(type = "qual",palette = 3) +
  labs(y="", x ="Percent of ads", fill = "Topic") +
    theme_bw()

merged %>%
  group_by(party, financial, chamber) %>%
  dplyr::count(party, financial, topic, chamber) %>%
  mutate(prop = n / sum(n)) %>%
  filter(party %in% c("DEMOCRAT","REPUBLICAN")) %>%
  mutate(post = glue("{financial} posted by {party}")) %>%
  ggplot(aes(x=prop*100,y=post,fill=fct_rev(topic))) + 
  geom_bar(stat="identity",alpha=.7, width = .4) +
  scale_fill_brewer(type = "qual",palette = 3) +
  facet_grid(~chamber) +
  labs(y="", x ="Percent of ads", fill = "Ad theme / topic") +
  theme_bw()




#### Background plots #######################################################

summary(key_dfm)

visualize_keywords(docs = key_dfm, keywords = keywords_stemmed[1:2])
visualize_keywords(docs = key_dfm, keywords = keywords_stemmed[3:5])
visualize_keywords(docs = key_dfm, keywords = keywords_stemmed[c(6,9)])
visualize_keywords(docs = key_dfm, keywords = keywords_stemmed[c(3,4,5,9)])
visualize_keywords(docs = key_dfm, keywords = keywords_stemmed[c(1,7,8)])

key_viz <- visualize_keywords(docs = key_dfm, keywords = keywords_stemmed)
key_viz
values_fig(key_viz) %>% View()

values_fig(key_viz) %>% filter(Ranking <=5)

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
  scale_y_continuous(expand = c(0, 0), limits = c(0, 0.35), labels = scales::percent) +
  theme_bw() +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_blank()) +
  labs(x = NULL, y = expression(theta))
