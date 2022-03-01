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
  Race = c("anti-discrimin",
                      "discrimin","discriminatori",
                      "racism","racial","anti-racist",
                      "bias",
                      "#criminaljusticereform",
                      "racialprofil",
                      "floyd","#georgefloyd"),
  Immigration = c("immigr","immigr",
                  "border",
                  "migrant","migrat",
                  "dreamer","daca","#daca"),
  Environment = c("earth","climat","weather","oil","drill",
                  "environ","environment",
                  "fossil_fuel",
                  "green_new_deal",
                  "temperatur"),
  President = c("trump","presid","@realdonaldtrump","trump-republican"),
  Patriotic   = c("america", "usa", "nation","country","patriot","#teamusa"),
  # Foreign_Affairs = c("foreign","global",
  #                    "china","russia","iran","war","afghanistan",
  #                    "macron","merkel"),
  Education = c("educ","school","academi","univers","tuition",
                "student",
                "student_debt"))


dfmHouse <- dfm_FB_ad_stemmed %>%
                dfm_subset(chamber=="house")

# Necessary to drop non-textual matrix rows:
dfmHouseAnalaysis <- dfm_subset(dfmHouse,ntoken(dfmHouse) > 0)

# Add IDs to the matrix
dfmHouseAnalaysis$tempID <- 1:nrow(dfmHouseAnalaysis)  


key_dfm_House <- keyATM_read(texts = dfmHouseAnalaysis)

summary(key_dfm_House)

visualize_keywords(docs = key_dfm_House, keywords = keywords_stemmed[1:2])
visualize_keywords(docs = key_dfm_House, keywords = keywords_stemmed[3:5])
visualize_keywords(docs = key_dfm_House, keywords = keywords_stemmed[c(6,8,9)])
visualize_keywords(docs = key_dfm_House, keywords = keywords_stemmed[c(7,10)])

key_viz <- visualize_keywords(docs = key_dfm_House, keywords = keywords_stemmed)
key_viz
values_fig(key_viz) %>% View()

values_fig(key_viz) %>% filter(Ranking <=5)


############
# BASE MODEL
############

out_stemmed <- keyATM(docs  = key_dfm_House,  
                      no_keyword_topics = 5,              
                      keywords          = keywords_stemmed,       # CHOOSE STEMMED or not
                      model             = "base",       
                      options           = list(seed = 81477))



# Extract top topics
TOPICS_base <- top_topics(out_stemmed)

# Add IDs to the output
TOPICS_base$tempID <- 1:nrow(TOPICS_base)  

# Show which words are most prognostic of topics:
top_words(out_stemmed)

# Check the objects for merging are of the same length
assertthat::are_equal(nrow(dfmHouseAnalaysis), nrow(TOPICS_base))

# Prepare a tibble based on the DFM attributes:
tibbleForAnalysis <- docvars(dfmHouseAnalaysis) %>% tibble()

merged <- left_join(tibbleForAnalysis, TOPICS_base, 
                    by ="tempID")

merged %>% count(Rank1)

merged <- merged %>% 
  mutate(topic = case_when(
    Rank1 == "1_Economy"    ~ "Economy and jobs",
    Rank1 == "2_Health"    ~ "Health care or public health",
    Rank1 == "9_Patriotic" ~ "Patriotic speech",
    #Rank1 == "11_Education" ~ "Education",
    #Rank1 == "5_Social_Issues_B"     ~ "Women's issues",
    Rank1 == "6_Immigration"     ~ "Immigration",
    Rank1 == "8_President"     ~ "Trump",
    !is.na(Rank1) ~ "Various other / mixed")
  )

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
  theme_bw() +
  labs(subtitle = "Topics of FB ads (2020 US House candidates)")

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
    theme_bw() +
  labs(subtitle = "Topics of FB ads (2020 US House candidates)")
