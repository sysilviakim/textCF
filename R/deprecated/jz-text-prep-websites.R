# renv::restore()
library(tidyverse)
library(here)
library(ggpubr)
library(quanteda)
library(quanteda.textplots)
library(quanteda.textstats) # devtools::install_github("kbenoit/quanteda.dictionaries")
library(quanteda.dictionaries)


# THESE ARE 2020 WEBSITES / candidate-driven data

load(here("data", "tidy", "cong_complete.Rda"))
load(here("data", "raw", "cong-filtered.Rda"))


# We need to extract the text from the following columsn:
#   - contribution_blurb
#   - text
# There is a little bit of text in these columns too in some cases "title"; "footer";


# How many entries have non-empty text fields:
cong_complete$house %>% tally()
cong_complete$house %>%
  filter(text != "") %>%
  tally()
cong_complete$house %>%
  filter(contribution_blurb != "") %>%
  tally()
cong_complete$house %>%
  filter(contribution_blurb != "" | text != "") %>%
  tally()

cong_complete$senate %>% tally()
cong_complete$senate %>%
  filter(text != "") %>%
  tally()
cong_complete$senate %>%
  filter(contribution_blurb != "" | text != "") %>%
  tally()

##################################
# SUBSETTING and extracting text:
# Generate an "ads_text" column
# Only keep rows with non-empty text columns:
##################################
cong_complete$senate <- cong_complete$senate %>%
  mutate(
    ads_text = case_when(
      text != "" ~ text,
      contribution_blurb != "" ~ contribution_blurb
    )
  ) %>%
  filter(!is.na(ads_text))

cong_complete$house <- cong_complete$house %>%
  mutate(
    ads_text = case_when(
      text != "" ~ text,
      contribution_blurb != "" ~ contribution_blurb
    )
  ) %>%
  filter(!is.na(ads_text))

#################
# PREPARE CORPORA
#################
corp_house <- corpus(cong_complete$house$ads_text,
  docvars =
    data.frame(
      party = cong_complete$house$party,
      chamber = rep("house", nrow(cong_complete$house))
    )
)

# Rename the documents, adding a House prefix:
docnames(corp_house) <- paste("House", 1:ndoc(corp_house), sep = "")

corp_senate <- corpus(cong_complete$senate$ads_text,
  docvars =
    data.frame(
      party = cong_complete$senate$party,
      chamber = rep("senate", nrow(cong_complete$senate))
    )
)

# Rename the documents, adding a Senate prefix:
docnames(corp_senate) <- paste("Senate", 1:ndoc(corp_senate), sep = "")

# We can now merge corpora b/c the document names are no longer overlapping:
corp <- corp_house + corp_senate

toks_congress <- tokens(corp) %>% tokens_tolower()

dfm_congress <- dfm(toks_congress)
