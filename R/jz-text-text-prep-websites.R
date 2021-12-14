# renv::restore() 

library(tidyverse)
library(here)
library(ggpubr)
library(quanteda)
library(quanteda.textplots)
library(quanteda.textstats)  # devtools::install_github("kbenoit/quanteda.dictionaries") 
library(quanteda.dictionaries)


# READ IN TEXT DATA:
# THESE ARE 2020 WEBSITES / candidate-driven data:
## this is what the motivated people see:

load(here("data", "tidy", "cong_complete.Rda"))


# Other suspected columns are:
# title; footer; 

# How many entries have non-empty text fields:

cong_complete$house %>% tally()
cong_complete$house %>% filter(text != "") %>% tally()

cong_complete$senate %>% tally()
cong_complete$senate %>% filter(text != "") %>% tally()


##################################
# SUBSETTING:
# Only keep rows with non-empty text columns:
##################################
cong_complete$senate <- cong_complete$senate %>% filter(text != "")
cong_complete$house <- cong_complete$house %>% filter(text != "")

corp_house <- corpus(cong_complete$house$text,
                     docvars = 
                       data.frame(
                         party = cong_complete$house$party,
                         chamber = rep("house",nrow(cong_complete$house))
                       ))

# Rename the documents, adding a House prefix:
docnames(corp_house) <- paste("House", 1:ndoc(corp_house), sep="")

corp_senate <- corpus(cong_complete$senate$text,
                     docvars = 
                       data.frame(
                         party = cong_complete$senate$party,
                         chamber = rep("senate",nrow(cong_complete$senate))
                       ))

# Rename the documents, adding a Senate prefix:
docnames(corp_senate) <- paste("Senate", 1:ndoc(corp_senate), sep="")

# We can now merge corpora b/c the document names are no longer overlapping:
corp <- corp_house + corp_senate

toks_congress <- tokens(corp) %>% tokens_tolower()

dfm_congress <- dfm(toks_congress)

