renv::restore() 

library(tidyverse)
library(here)
library(ggpubr)
library(quanteda)
library(quanteda.textplots)
library(quanteda.textstats)
# devtools::install_github("kbenoit/quanteda.dictionaries") 
library(quanteda.dictionaries)
# https://muellerstefan.net/files/quanteda-cheatsheet.pdf

# Read in the Moral foundations dictionary
MFD <- dictionary(file = "data/raw/dictionaries/mfd2.0.dic")
troll <- read_csv(file = "data/raw/dictionaries/troll_and_divide/troll_and_divide_Glove_Expansion_Raters_phase_2.csv") %>%
  pull(Word) %>%
  list(troll = .) %>%
  dictionary()


# THESE ARE 2020 WEBSITES / candidate-driven data:
## this is what the motivated people see:
load(here("data", "tidy", "cong_complete.Rda"))

# title; footer; 

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


lookup_trolling <- dfm_lookup(dfm_congress, troll)
lookup_MFD <- dfm_lookup(dfm_congress, MFD)

data_troll <- quanteda::convert(lookup_trolling,to="data.frame") %>%
  cbind(docvars(dfm_congress))

data_MFD <- quanteda::convert(lookup_MFD,to="data.frame") %>%
                      cbind(docvars(dfm_congress))

data_troll %>% 
  group_by(party, chamber) %>%
  summarise(average_trolling_words = mean(troll)) %>%
  ggplot(aes(x=party, y = average_trolling_words, fill = party)) +
  geom_col(alpha=.8, width = .5) +
  facet_grid(~ chamber) +
  scale_fill_manual(values = c("midnightblue","darkgreen","darkred")) +
  labs(x = "", 
       y = "Average number of dict. words per document",
  title = "Occurence or words from the trolling-and-incivility dictionary
broken down by Party and chamber of US Congress",
  subtitle = "Corpus: Candidate websites")
ggsave("fig/dict_trolling_websites.png")

# Summary for the US House Only
data_troll %>% 
  mutate(House = str_detect(doc_id,"House")) %>%
  filter(House == T) %>%
  group_by(party) %>%
  summarise(average_trolling_words = mean(troll))

# Summary for the US Senate Only
data_troll %>% 
  mutate(Senate = str_detect(doc_id,"Senate")) %>%
  filter(Senate == T) %>%
  group_by(party) %>%
  summarise(average_trolling_words = mean(troll))

###################
# MORAL FOUNDATIONS
###################

data_MFD %>%
  pivot_longer(cols = 2:11, 
               names_to = "moral_foundation", 
               values_to = "N") %>%
  group_by(party) %>%
  summarise(avg = mean(N))

data_MFD %>%
  pivot_longer(cols = 2:11, 
               names_to = "moral_foundation", 
               values_to = "N") %>%
  group_by(moral_foundation,party) %>%
  summarise(avg = mean(N))

data_MFD %>%
  pivot_longer(cols = 2:11, 
               names_to = "moral_foundation", 
               values_to = "N") %>%
  group_by(party, chamber) %>%
  summarise(avg = mean(N)) %>%
ggplot(aes(x=party, y = avg, fill = party)) +
  geom_col(alpha=.8, width = .5) +
  facet_grid(~ chamber) +
  scale_fill_manual(values = c("midnightblue","darkgreen","darkred")) +
  labs(x = "", 
       y = "Average number of dict. words per document",
       title = "Occurence or words from the Moral Foundations dictionary
broken down by Party and chamber of US Congress",
       subtitle = "Corpus: Candidate websites")
ggsave("fig/dict_MFD_websites.png")

data_MFD %>% 
  pivot_longer(cols = 2:11, 
               names_to = "moral_foundation", 
               values_to = "N") %>%
  filter(N >=1,
         party=="REPUBLICAN") %>%
  ggplot(aes(x= N)) +
  geom_histogram() +
  facet_wrap(~ moral_foundation) +
  labs(
       x = "Moral foundation words used on candidates websistes",
       y = "Word count",
       subtitle = "Corpus: US House and Senate Republicans") 
 ggsave("fig/dict_MFD_by_dimension_websites_GOP.png")

