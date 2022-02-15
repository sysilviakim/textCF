# Setup ========================================================================
source(here::here("R", "utilities.R"))

# Load data ====================================================================

## Text is in "ad_creative_body"
load(here("data", "tidy", "fb_unique.Rda"))
load(here("output", "fb_quanteda.Rda"))

# Course of Action =============================================================

# For the project, we want to get plots and figures pertaining to candidates'
# usage of:
### Emotionally Charged Rhetoric
### Trolling Words
### Moral Foundations?

## It should definitely be noted that the overwhelming majority of the code here
## is originally Jan's and Silvia's -- credit must go where it's due!
## My additions will generally be adding in new variables as we gather them, or
## making minor tweaks here and there.

## While this probably won't be the final analysis, let's get some code written
## up.

# Emotionally Charged Rhetoric =================================================

library(lingmatch)
NRC <- read.dic(here("data", "raw", "dictionaries", "NRC.dic"))
NRC <- dictionary(NRC)

senate_data <- fb_unique[["senate"]]
house_data <- fb_unique[["house"]]
# Now, there are all sorts of different corpuses we could gather to perform the
# sentiment analyses on, including all manner of variables. 
# We could separate out winners and losers (based on vote_share, assuming that
# anything greater than 0.5 constitutes a victory...
# What I have below is just one example of what we might do:

# 1: Party comparisons in Senate
senate_gop <- senate_data[senate_data$party == "REPUBLICAN", ]
senate_dem <- senate_data[senate_data$party == "DEMOCRAT", ]
# For some reason, this generates a bunch of rows that are all NAs...let's fix
# that with the following:
senate_gop <- senate_gop[rowSums(is.na(senate_gop)) != ncol(senate_gop), ]
senate_dem <- senate_dem[rowSums(is.na(senate_dem)) != ncol(senate_dem), ]
s_gop_corpus <- corpus(senate_gop %>% select(
  name = candidate, 
  text = ad_creative_body))
s_dem_corpus <- corpus(senate_dem %>% select(
  name = candidate, 
  text = ad_creative_body))

s_gop_Lexicoder <- liwcalike(s_gop_corpus, dictionary = NRC)
#s_gop_MFD <- liwcalike(c2, dictionary = MFD)
s_dem_Lexicoder <- liwcalike(s_dem_corpus, dictionary = NRC)
#s_dem_MFD <- liwcalike(c1, dictionary = MFD)

(
  F1 <- s_gop_Lexicoder %>% 
    filter(Dic > 0) %>%
    mutate(index = row_number()) %>%
    select(index, Dic, anger, anticipation, disgust, fear, joy, negative, 
           positive, sadness, surprise, trust) %>%
    mutate(sum = rowSums(across(anger:trust)) / Dic) %>%
    select(index, anger, disgust, fear, negative, positive) %>%
    mutate(other = 100 - anger - disgust - fear - negative - positive) %>%
    pivot_longer(cols = anger:other) %>%
    ggplot(aes(y = fct_reorder(factor(index),value), x = value, color = name, 
               fill = name)) +
    geom_col() +
    jcolors::scale_fill_jcolors(palette = "pal4") +
    jcolors::scale_color_jcolors(palette = "pal4") +
    labs(x="Proportion of Classified Words", y = "", 
         title = "NRC Language Analysis of Senate Republican Statements",
         color = "", fill = "") +
    theme(axis.text.y=element_blank())
)

(
  F2 <- s_dem_Lexicoder %>% 
    filter(Dic > 0) %>%
    mutate(index = row_number()) %>%
    select(index, Dic, anger, anticipation, disgust, fear, joy, negative, 
           positive, sadness, surprise, trust) %>%
    mutate(sum = rowSums(across(anger:trust)) / Dic) %>%
    select(index, anger, disgust, fear, negative, positive) %>%
    mutate(other = 100 - anger - disgust - fear - negative - positive) %>%
    pivot_longer(cols = anger:other) %>%
    ggplot(aes(y = fct_reorder(factor(index),value), x = value, color = name, 
               fill = name)) +
    geom_col() +
    jcolors::scale_fill_jcolors(palette = "pal4") +
    jcolors::scale_color_jcolors(palette = "pal4") +
    labs(x="Proportion of Classified Words", y = "", 
         title = "NRC Language Analysis  of Senate Democrat Statements",
         color = "", fill = "") +
    theme(axis.text.y=element_blank())
)
# Something's up here...

# 2: Senate Financial/Non-Financial, By Party

senate_gop_f <- senate_gop[senate_gop$financial == "Financial", ]
senate_gop_nf <- senate_gop[senate_gop$financial == "Non-Financial", ]
senate_dem_f <- senate_dem[senate_dem$financial == "Financial", ]
senate_dem_nf <- senate_dem[senate_dem$financial == "Non-Financial", ] 
# No extraneous NA rows generated here

senate_gop_f_corpus <- corpus(senate_gop_f %>% select(
  name = candidate, 
  text = ad_creative_body))
senate_gop_nf_corpus <- corpus(senate_gop_nf %>% select(
  name = candidate, 
  text = ad_creative_body))
senate_dem_f_corpus <- corpus(senate_dem_f %>% select(
  name = candidate, 
  text = ad_creative_body))
senate_dem_nf_corpus <- corpus(senate_dem_nf %>% select(
  name = candidate, 
  text = ad_creative_body))

s_gop_f_Lexicoder <- liwcalike(senate_gop_f_corpus, dictionary = NRC)
s_gop_nf_Lexicoder <- liwcalike(senate_gop_nf_corpus, dictionary = NRC)
s_dem_f_Lexicoder <- liwcalike(senate_dem_f_corpus, dictionary = NRC)
s_dem_nf_Lexicoder <- liwcalike(senate_dem_nf_corpus, dictionary = NRC)

(
  F3 <- s_gop_f_Lexicoder %>% 
    filter(Dic > 0) %>%
    mutate(index = row_number()) %>%
    select(index, Dic, anger, anticipation, disgust, fear, joy, negative, 
           positive, sadness, surprise, trust) %>%
    mutate(sum = rowSums(across(anger:trust)) / Dic) %>%
    select(index, anger, disgust, fear, negative, positive) %>%
    mutate(other = 100 - anger - disgust - fear - negative - positive) %>%
    pivot_longer(cols = anger:other) %>%
    ggplot(aes(y = fct_reorder(factor(index),value), x = value, color = name, 
               fill = name)) +
    geom_col() +
    jcolors::scale_fill_jcolors(palette = "pal4") +
    jcolors::scale_color_jcolors(palette = "pal4") +
    labs(x="Proportion of Classified Words", y = "", 
         title = "NRC Analysis of Senate Republican Financial Statements",
         color = "", fill = "") +
    theme(axis.text.y=element_blank())
)

(
  F4 <- s_gop_nf_Lexicoder %>% 
    filter(Dic > 0) %>%
    mutate(index = row_number()) %>%
    select(index, Dic, anger, anticipation, disgust, fear, joy, negative, 
           positive, sadness, surprise, trust) %>%
    mutate(sum = rowSums(across(anger:trust)) / Dic) %>%
    select(index, anger, disgust, fear, negative, positive) %>%
    mutate(other = 100 - anger - disgust - fear - negative - positive) %>%
    pivot_longer(cols = anger:other) %>%
    ggplot(aes(y = fct_reorder(factor(index),value), x = value, color = name, 
               fill = name)) +
    geom_col() +
    jcolors::scale_fill_jcolors(palette = "pal4") +
    jcolors::scale_color_jcolors(palette = "pal4") +
    labs(x="Proportion of Classified Words", y = "", 
         title = "NRC Analysis  of Senate Republican Non-Financial Statements",
         color = "", fill = "") +
    theme(axis.text.y=element_blank())
)

(
  F5 <- s_dem_f_Lexicoder %>% 
    filter(Dic > 0) %>%
    mutate(index = row_number()) %>%
    select(index, Dic, anger, anticipation, disgust, fear, joy, negative, 
           positive, sadness, surprise, trust) %>%
    mutate(sum = rowSums(across(anger:trust)) / Dic) %>%
    select(index, anger, disgust, fear, negative, positive) %>%
    mutate(other = 100 - anger - disgust - fear - negative - positive) %>%
    pivot_longer(cols = anger:other) %>%
    ggplot(aes(y = fct_reorder(factor(index),value), x = value, color = name, 
               fill = name)) +
    geom_col() +
    jcolors::scale_fill_jcolors(palette = "pal4") +
    jcolors::scale_color_jcolors(palette = "pal4") +
    labs(x="Proportion of Classified Words", y = "", 
         title = "NRC Analysis of Senate Democrat Financial Statements",
         color = "", fill = "") +
    theme(axis.text.y=element_blank())
)

(
  F6 <- s_dem_nf_Lexicoder %>% 
    filter(Dic > 0) %>%
    mutate(index = row_number()) %>%
    select(index, Dic, anger, anticipation, disgust, fear, joy, negative, 
           positive, sadness, surprise, trust) %>%
    mutate(sum = rowSums(across(anger:trust)) / Dic) %>%
    select(index, anger, disgust, fear, negative, positive) %>%
    mutate(other = 100 - anger - disgust - fear - negative - positive) %>%
    pivot_longer(cols = anger:other) %>%
    ggplot(aes(y = fct_reorder(factor(index),value), x = value, color = name, 
               fill = name)) +
    geom_col() +
    jcolors::scale_fill_jcolors(palette = "pal4") +
    jcolors::scale_color_jcolors(palette = "pal4") +
    labs(x="Proportion of Classified Words", y = "", 
         title = "NRC Analysis  of Senate Democrat Non-Financial Statements",
         color = "", fill = "") +
    theme(axis.text.y=element_blank())
)
# Something's going on with this one...

# In any case, should not just have fancy figures -- should also have some
# detailed tables to present the main thrust of the results.
# What sort of analysis should that be? A simple difference-in-means? Some form
# of regression? Should talk about this with Silvia and Jan in the next meeting.
  
# Trolling Words ===============================================================

# Reading in the Trolling dictionary:
troll <- read_csv(
  file = here(
    "data", "raw", "dictionaries",
    "troll_and_divide/troll_and_divide_Glove_Expansion_Raters_phase_2.csv"
  )
) %>%
  pull(Word) %>%
  list(troll = .) %>%
  dictionary()

lookup_trolling <- dfm_lookup(dfm_FB_ad, troll)
lookup_trolling_prop <- dfm_lookup(dfm_FB_ad_prop, troll)
CANDlookup_trolling <- dfm_lookup(dfm_FB_cand, troll)
CANDlookup_trollingPROP <- dfm_lookup(dfm_FB_cand_prop, troll)

CANDdata_troll <- quanteda::convert(CANDlookup_trolling, to = "data.frame") %>%
  cbind(docvars(dfm_FB_cand))

CANDdata_trollPROP <-
  quanteda::convert(CANDlookup_trollingPROP, to = "data.frame") %>%
  cbind(docvars(dfm_FB_cand_prop))


# Moral Foundations ============================================================

# General notes ================================================================

## simple_cap is in Kmisc package...why does loading it in not fix the error?

# Useful other charts/figures ==================================================
# A
