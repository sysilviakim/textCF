# renv::restore() 

library(tidyverse)
library(here)
library(ggpubr)
library(quanteda)

# READ IN TEXT DATA:

load(here("data/raw/fb/fb-house-raw-ads-2020.rda"))
load(here("data/raw/fb/fb-senate-raw-ads-2020.rda"))

FBH <- fb_house %>% map_dfr("tbl", .id = NULL)
FBS <- fb_senate %>% map_dfr("tbl", .id = NULL)

rm(fb_house)
rm(fb_senate)

# Text is in "ad_creative_body":

FBH %>% filter(!is.na(ad_creative_body)) %>%
          filter(ad_creative_link_caption == "secure.actblue.com" |
                 ad_creative_link_caption == "secure.winred.com" | 
               is.na(ad_creative_link_caption)) %>%
  group_by(ad_creative_link_caption) %>%
  sample_n(10) %>%
  select(ad_creative_link_caption,ad_creative_body)

FBH_unique <- 
  FBH %>% 
  select(page_id,
         page_name,
         ad_creative_body,
         ad_creative_link_caption) %>%
  distinct() 

FBS_unique <- 
  FBS %>% 
  select(page_id,
         page_name,
         ad_creative_body,
         ad_creative_link_caption) %>%
  distinct()


covariates_house <- read_csv(here("data/raw/fb/fb-house.csv")) %>%
  mutate(voteshare = candidatevotes / totalvotes) %>%
  select(fb_ad_library_id,
         candidate,
         voteshare,
         party,
         office,
         state_po,
         year)

covariates_senate <- read_csv(here("data/raw/fb/fb-senate.csv")) %>%
  mutate(voteshare = candidatevotes / totalvotes) %>%
  select(fb_ad_library_id,
         candidate,
         voteshare,
         party_simplified,
         office,
         state_po,
         year)

# Merge in covariates [but note that we need page_ids to be strings]
FBS_forAnalysis <- left_join(FBS_unique, 
                  covariates_senate %>% mutate(page_id = as.character(fb_ad_library_id))) %>%
                                        filter(ad_creative_body != "")

FBH_forAnalysis <- left_join(FBH_unique, 
                 covariates_house %>% mutate(page_id = as.character(fb_ad_library_id))) %>%
                                      filter(ad_creative_body != "")


FBS_forAnalysis <- FBS_forAnalysis %>%
  mutate(type = case_when(
    ad_creative_link_caption == "secure.actblue.com" ~ "ActBlue",
    ad_creative_link_caption == "secure.winred.com" ~ "WinRed",
    is.na(ad_creative_link_caption) ~ "Non-financial"
  ))

FBH_forAnalysis <- FBH_forAnalysis %>%
  mutate(type = case_when(
    ad_creative_link_caption == "secure.actblue.com" ~ "ActBlue",
    ad_creative_link_caption == "secure.winred.com" ~ "WinRed",
    is.na(ad_creative_link_caption) ~ "Non-financial"
  ))

# FBH %>% filter(!is.na(ad_creative_body)) %>%
#   filter(ad_creative_link_caption == "secure.actblue.com" |
#            ad_creative_link_caption == "secure.winred.com" | 
#            is.na(ad_creative_link_caption)) %>%
#   group_by(ad_creative_link_caption) %>%
#   sample_n(10) %>%
#   select(ad_creative_link_caption,ad_creative_body) %>% View()

################
# BUILD A CORPUS
################
CS <- corpus(FBS_forAnalysis$ad_creative_body,
             docvars = 
               data.frame(
                 type = FBS_forAnalysis$type,
                 candidate = FBS_forAnalysis$candidate,
                 party = FBS_forAnalysis$party_simplified,
                 voteshare = FBS_forAnalysis$voteshare,
                 chamber = rep("senate",nrow(FBS_forAnalysis))
               ))
# Rename the documents, adding a Senate prefix:
docnames(CS) <- paste("Senate", 1:ndoc(CS), sep="")

CH <- corpus(FBH_forAnalysis$ad_creative_body,
       docvars = 
         data.frame(
           type = FBH_forAnalysis$type,
           candidate = FBH_forAnalysis$candidate,
           party = FBH_forAnalysis$party,
           voteshare = FBH_forAnalysis$voteshare,
           chamber = rep("house",nrow(FBH_forAnalysis))
         ))

# Rename the documents, adding a House prefix:
docnames(CH) <- paste("House", 1:ndoc(CH), sep="")


CORP_FB <- CS + CH

####################
# TOKENIZE DOCUMENTS
####################
toks_FB <- tokens(CORP_FB) %>% 
  tokens(remove_url = TRUE,
         remove_punct=TRUE,
         include_docvars = TRUE) %>% 
      #  tokens_wordstem() %>%
      tokens_remove(stopwords("english")) %>% 
      tokens_remove(stopwords("spanish")) %>%
      tokens_remove(c("rt","amp","u8")) %>%
      tokens_tolower()

##############################
# Each document will be an AD
##############################
dfm_FB <- dfm(toks_FB)
dfm_FBprop <- dfm_weight(dfm_FB, scheme = "prop")

###################################
# Each document will be a candidate
###################################
dfmCAND <- dfm_group(dfm_FB, groups = candidate)
dfmCANDprop <- dfm_weight(dfmCAND, scheme = "prop")



