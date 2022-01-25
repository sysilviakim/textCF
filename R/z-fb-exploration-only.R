# renv::restore() 

library(tidyverse)
library(here)
library(ggpubr)
library(quanteda)
library(quanteda.textplots)
library(quanteda.textstats)  
library(quanteda.dictionaries)


load("data/raw/fb/fb-house-raw-ads-2020.rda")
load("data/raw/fb/fb-senate-raw-ads-2020.rda")

# Text is in "ad_creative_body" 

FBH <- fb_house %>% map_dfr("tbl", .id = NULL)




FBH %>% filter(!is.na(ad_creative_body)) %>%
  filter(ad_creative_link_caption == "secure.actblue.com" |
           ad_creative_link_caption == "secure.winred.com" | 
           is.na(ad_creative_link_caption)) %>%
  group_by(ad_creative_link_caption) %>%
  sample_n(10) %>%
  select(ad_creative_link_caption,ad_creative_body)


# FBH %>% filter(!is.na(ad_creative_body)) %>%
#   filter(ad_creative_link_caption == "secure.actblue.com" |
#            ad_creative_link_caption == "secure.winred.com" | 
#            is.na(ad_creative_link_caption)) %>%
#   group_by(ad_creative_link_caption) %>%
#   sample_n(10) %>%
#   select(ad_creative_link_caption,ad_creative_body) %>% View()












FBH$wordTrump <- ifelse(str_detect(str_to_lower(FBH$ad_creative_body), 
                                   "trump"),1,0)



FBH$wordsCovid <- ifelse(str_detect(str_to_lower(FBH$ad_creative_body), 
                                    paste(c("covid","#covid","covid-19","#covid19",
                                            "coronavirus","virus",
                                            "infection","infected",
                                            "vaccine","vaccines"),collapse = '|')),1,0)


FBH_unique <- 
  FBH %>% 
  select(page_name,ad_creative_body,wordTrump,wordsCovid) %>%
  distinct() 

FBH_unique %>%
  group_by(page_name) %>%
  tally() %>%
  arrange(-n)

top30Housepage <- FBH_unique %>%
  group_by(page_name) %>%
  tally() %>%
  arrange(-n) %>%
  slice(1:30)

congressTrumpTop30 <- 
  FBH_unique %>%
  group_by(page_name,wordTrump) %>%
  tally() %>%
  filter(page_name %in% top30Housepage$page_name)

congressCovidTop30 <- 
  FBH_unique %>%
  group_by(page_name,wordsCovid) %>%
  tally() %>%
  filter(page_name %in% top30Housepage$page_name)


congressTrumpTop30 %>%
  mutate(mTrump = case_when(
    wordTrump==0 ~ "Doesn't mention Trump",
    wordTrump==1 ~ "Mentioned Trump"
  )) %>%
  filter(!is.na(wordTrump)) %>%
  ggplot(aes(x=n,
             y=fct_reorder(page_name,n),
             fill=fct_rev(mTrump))) +
  geom_col() +
  scale_fill_brewer(type = "qual", palette = 6) +
  labs(fill = "", x = "Number of ads",
       y = "",
       title = "Frequency of Trump mentions in Facebook ads",
       subtitle = "Top political 30 ad producers (US House)")



congressCovidTop30 %>%
  mutate(cw = case_when(
    wordsCovid==0 ~ "Doesn't mention Covid",
    wordsCovid==1 ~ "Mentioned Covid"
  )) %>%
  filter(!is.na(wordsCovid)) %>%
  ggplot(aes(x=n,
             y=fct_reorder(page_name,n),
             fill=fct_rev(cw))) +
  geom_col() +
  scale_fill_brewer(type = "qual", palette = 1) +
  labs(fill = "", x = "Number of ads",
       y = "",
       title = "Mentions of Covid / virus / infections",
       subtitle = "Top political 30 ad producers (US House)") +
  theme_bw()



FBH %>% filter(page_name == "Alexandria Ocasio-Cortez") %>%
  group_by(ad_creation_time) %>%
  tally() %>%
  ggplot(aes(x=ad_creation_time,y=n)) +
  geom_point() +
  geom_line() +
  labs(subtitle = "Alexandria Ocasio-Cortez",
       title = "Activity (Number of ads) prior to filtering for duplicate texts")

FB_tib1 <- fb_house[[1]]$tbl
FB_tib2 <- fb_house[[2]]$tbl

# 2 ways to tag speakers: [page_id is inclusive; page_name is a useful! split]
FB_tib2 %>% group_by(page_name) %>%
  tally()
FB_tib2 %>% group_by(page_id) %>%
  tally()


fb_senate[[12]]$tbl %>% group_by(page_name) %>%
  tally()

# Do they link to Breitbart?
FB_tib2 %>% group_by(ad_creative_link_caption) %>%
  tally() %>%
  arrange(-n)
  

FB_tib1 %>% group_by(ad_creation_time) %>%
  tally() %>%
  ggplot(aes(x=ad_creation_time,y=n)) +
  geom_point() +
  geom_line() +
  labs(subtitle = "Cori Bush",
       title = "Number of ads")


FB_tib1 %>% 
  select(ad_creative_body) %>%
  distinct()
