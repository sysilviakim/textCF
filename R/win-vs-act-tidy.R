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

load(here("data/tidy/actblue_federal_2022.Rda"))
load(here("data/tidy/winred_federal_2022.Rda"))

names(actblue_federal)
names(winred_federal)

# Read in the Moral foundations dictionary
MFD <- dictionary(file = "data/raw/dictionaries/mfd2.0.dic")


# Get text and other data from WinRed
c2 <- corpus(winred_federal %>% select(name, office, text))
# Documents need distinct names
docnames(c2) <- paste0("winred",seq(1:length(c2)))

c2$platform <- "WinRed"


# Get text from ActBlue
c1 <- corpus(actblue_federal %>% select(text = contribution_blurb,
                                        name = display_name))
c1$platform <- "ActBlue"

corp <- c1 + c2


################################################################
# Let's code texts on the basis of these dictionaries
################################################################
# Affective Norms for English Words (AFINN)
both_AFINN <- liwcalike(corp, dictionary = data_dictionary_AFINN)
winred_AFINN <- liwcalike(c2, dictionary = data_dictionary_AFINN)
actblue_AFF <- liwcalike(c1, dictionary = data_dictionary_AFINN)

# Explanation of the output:
# WC = word count BUT we checked that this counts punctuation too (unless punct. is removed from the corpus)
# WPS = Words per sentence
# Dic = percentage of words in the dictionary
# Exclam = count of "!"
# QMark = count of "?"


winred_AFINN %>% 
  filter(Dic > 0) %>%
  mutate(index = row_number()) %>%
  mutate(prop_negative = negative / Dic * 100) %>%
  select(prop_negative,index) %>%
  ggplot(aes(x = prop_negative, y = fct_reorder(factor(index),prop_negative))) +
  geom_col()

winred_AFINN  %>% 
  filter(Dic > 0) %>%
  mutate(prop_negative = negative / Dic * 100) %>%
  ggplot(aes(x=prop_negative)) +
  geom_histogram() +
  labs(title = "WinRed ads",
       x = "% negative [Affective Norms for English Words]",
       y = "Number of ads") +
  theme_minimal()

mean(winred_Lexicoder$Dic)

###############
# OTHER OPTIONS
###############
actblue_Lexicoder <- liwcalike(c1, dictionary = data_dictionary_NRC)
actblue_MFD <- liwcalike(c1, dictionary = MFD)
winred_Lexicoder <- liwcalike(c2, dictionary = data_dictionary_NRC)
winred_MFD <- liwcalike(c2, dictionary = MFD)

winred_Lexicoder %>% 
  filter(Dic > 0) %>%
  summarise(mean_Anger = mean(anger),
            mean_Fear = mean(fear),
            mean_Negative = mean(negative),
            mean_Positive = mean(positive)) %>%
  knitr::kable()


#############################################
# HEATMAP BASED ON THE NRC DICTIONARY
#############################################
(
  F1 <- winred_Lexicoder %>% 
  filter(Dic > 0) %>%
  mutate(index = row_number()) %>%
  select(index, Dic, anger, anticipation, disgust, fear, joy, negative, positive, sadness, surprise, trust) %>%
  mutate(sum = rowSums(across(anger:trust)) / Dic) %>%
  select(index, anger, disgust, fear, negative, positive) %>%
  mutate(other = 100 - anger - disgust - fear - negative - positive) %>%
  pivot_longer(cols = anger:other) %>%
  ggplot(aes(y = fct_reorder(factor(index),value), x = value, color = name, fill = name)) +
  geom_col() +
  jcolors::scale_fill_jcolors(palette = "pal4") +
  jcolors::scale_color_jcolors(palette = "pal4") +
  labs(x="Proportion of (classified) words", y = "", title = "Language analysis summary of WinRed ads",
       color = "", fill = "") +
  theme(axis.text.y=element_blank())
)

( 
F2 <- actblue_Lexicoder %>% 
  filter(Dic > 0) %>%
  mutate(index = row_number()) %>%
  select(index, Dic, anger, anticipation, disgust, fear, joy, negative, positive, sadness, surprise, trust) %>%
  mutate(sum = rowSums(across(anger:trust)) / Dic) %>%
  select(index, anger, disgust, fear, negative, positive) %>%
  mutate(other = 100 - anger - disgust - fear - negative - positive) %>%
  pivot_longer(cols = anger:other) %>%
  ggplot(aes(y = fct_reorder(factor(index),value), x = value, color = name, fill = name)) +
  geom_col() +
  jcolors::scale_fill_jcolors(palette = "pal4") +
  jcolors::scale_color_jcolors(palette = "pal4") +
  labs(x="Proportion of (classified) words", y = "", title = "Language analysis summary of Actblue ads",
       color = "", fill = "") +
  theme(axis.text.y=element_blank())
)


ggarrange(F1,F2)

BOTH_Lexicoder <- actblue_Lexicoder %>% mutate(source="Actblue") %>%
  bind_rows(winred_Lexicoder %>% mutate(source="WinRed")) %>%
  mutate(anger_fear_disgust_negative = (anger + fear +  disgust + negative) / Dic)

BOTH_Lexicoder %>%
  filter(Dic > 0) %>%
  mutate(over10 = ifelse(anger_fear_disgust_negative > .5,1,0)) %>%
  group_by(source) %>%
  summarise(mean_over10 = mean(over10))

BOTH_Lexicoder %>%
  filter(Dic > 0) %>%
  ggplot(aes(x=anger_fear_disgust_negative)) +
  geom_histogram() +
  facet_wrap(~source)


####################################
# Tokenize the corpus
####################################

toks <- c2 %>% 
  tokens(remove_url = TRUE,
         remove_punct=TRUE) %>% 
  tokens_remove(stopwords("english")) %>% 
  tokens_remove(c("rt","amp","u8","<p>","<",">","div","img","alt","br",
                  "text-align","li","=","b","nbsp","style",
                  "em","p","strong",
                  "center","u",
                  "href","rel")) %>%
  tokens_tolower()


# Document feature matrix:
DFM  <- dfm(toks)
DFM_trimmed <- dfm_trim(DFM, min_termfreq = 10)
# Make an FCM:
FCmat_trimmed <- fcm(DFM_trimmed)


# [This part is experimental]
# Matching tokens to dictionaries AFTER pre-processing:
dfmat_lookup1 <- dfm_lookup(DFM, dictionary = data_dictionary_AFINN)
dfmat_lookup2 <- dfm_lookup(DFM, dictionary = data_dictionary_NRC)


# Count the number of word occurences:
DFM[,c("democrat")] %>% sum()
DFM[,c("fight")] %>% sum()
DFM[,c("trump")] %>% sum()
DFM[,c("blm")] %>% sum()
DFM[,c("antifa")] %>% sum()

# topF <- textstat_frequency(DFM, n = 25, groups = "platform")


# Keyword in context:
kwic(c2, pattern="trump")

# FCM NETWORK PLOT

textplot_network(FCmat_trimmed, min_freq = 0.8) +
  labs(title = "Co-occurence of the common words in the WinRed dataset")
