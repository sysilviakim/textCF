source(here::here("R", "utilities.R"))

# Load data and create unique dataframe ========================================
## Text is in "ad_creative_body"
load(here("data", "tidy", "fb_matched.Rda"))
fb_matched$senate <- fb_matched$senate %>% rename(party = party_simplified)

## Deduplicate ads if body is the same
fb_unique <- fb_matched %>%
  map(
    ~ .x %>%
      select(
        fb_ad_library_id, page_name, party, inc, state_po,
        ad_creative_body, ad_creative_link_caption
      ) %>%
      distinct() %>%
      filter(ad_creative_body != "") %>%
      ## Antonio Delgado duplicates appear (House, with wrong PID)
      ## Drop these rows
      filter(!(page_name == "Antonio Delgado" & party == "REPUBLICAN")) %>%
      ## Label ad types by conduit/platform
      mutate(ad_creative_link_caption = tolower(ad_creative_link_caption)) %>%
      rowwise() %>% 
      mutate(
        type = case_when(
          grepl(ad_creative_link_caption, "secure.actblue.com") ~ "ActBlue",
          grepl(ad_creative_link_caption, "secure.winred.com") ~ "WinRed",
          grepl(ad_creative_link_caption, "secure.ngpvan.com") ~ "NGP VAN",
          is.na(ad_creative_link_caption) ~ "Non-financial"
        )
      ) %>%
      ungroup()
  )


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
      chamber = rep("senate", nrow(FBS_forAnalysis))
    )
)
# Rename the documents, adding a Senate prefix:
docnames(CS) <- paste("Senate", 1:ndoc(CS), sep = "")

CH <- corpus(FBH_forAnalysis$ad_creative_body,
  docvars =
    data.frame(
      type = FBH_forAnalysis$type,
      candidate = FBH_forAnalysis$candidate,
      party = FBH_forAnalysis$party,
      voteshare = FBH_forAnalysis$voteshare,
      chamber = rep("house", nrow(FBH_forAnalysis))
    )
)

# Rename the documents, adding a House prefix:
docnames(CH) <- paste("House", 1:ndoc(CH), sep = "")


CORP_FB <- CS + CH

####################
# TOKENIZE DOCUMENTS
####################
toks_FB <- tokens(CORP_FB) %>%
  tokens(
    remove_url = TRUE,
    remove_punct = TRUE,
    include_docvars = TRUE
  ) %>%
  #  tokens_wordstem() %>%
  tokens_remove(stopwords("english")) %>%
  tokens_remove(stopwords("spanish")) %>%
  tokens_remove(c("rt", "amp", "u8")) %>%
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
