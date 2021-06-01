library(tidyverse)
library(here)
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

# TO DO: READ IN ACTBLUE
# act_blurb <- actblue_federal$js_rest$contribution_blurb

# Get text from ActBlue
# c1 <- corpus(act_blurb)
# c1$platform <- "ActBlue"
# 
# corp <- c1 + c2


########################################################################
# Let's code texts on the basis of these dictionaries
########################################################################
# Affective Norms for English Words (AFINN)
winred_AFINN <- liwcalike(c2, dictionary = data_dictionary_AFINN)
winred_Lexicoder <- liwcalike(c2, dictionary = data_dictionary_NRC)
winred_MFD <- liwcalike(c2, dictionary = MFD)


# Explanation of the output:
# WC = word count BUT we checked that this counts punctuation too (unless punct. is removed from the corpus)
# WPS = Words per sentence
# Dic = percentage of words in the dictionary
# Exclam = count of "!"
# QMark = count of "?"



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

winred_Lexicoder %>% 
  filter(Dic > 0) %>%
  summarise(mean_Anger = mean(anger),
            mean_Fear = mean(fear),
            mean_Negative = mean(negative),
            mean_Positive = mean(positive)) %>%
  knitr::kable()


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

dfmat_lookup1 <- dfm_lookup(DFM, dictionary = data_dictionary_AFINN)
dfmat_lookup2 <- dfm_lookup(DFM, dictionary = data_dictionary_NRC)


DFM[,c("democrat")] %>% sum()
DFM[,c("fight")] %>% sum()
DFM[,c("trump")] %>% sum()
DFM[,c("blm")] %>% sum()
DFM[,c("antifa")] %>% sum()

# topF <- textstat_frequency(DFM, n = 25, groups = "platform")

winred_text %>% filter(!is.na(name)) %>% group_by(name) %>% tally() %>% 
  arrange(n) %>% slice(1:50)


kwic(c2, pattern="trump")


# FCM NETWORK PLOT

textplot_network(FCmat_trimmed, min_freq = 0.8) +
  labs(title = "Co-occurence of the common words in the WinRed dataset")
