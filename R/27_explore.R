library(tidyverse)
library(quanteda)
library(quanteda.textplots)
library(quanteda.textstats)


load("data/raw/2022/actblue/actblue_js_scraped.Rda")

names(actblue_js_full)

# MISC DATA GRABS:
# entitites <- actblue_js_full$entities
# entitites[[1]]
# names(entitites[[1]])
# 
# share_content <- actblue_js_full$share_content
# 
# twitter_text <- map(share_content,"twitter_text") %>% unlist()
# 
# # map(entitites, "recurring_enabled") %>% unlist() %>% head()
# # map(entitites, "display_name") %>% unlist() %>% head()
# 
# act <- actblue_js_full %>% select(url,entities,radio_amounts)
# names(act$entities)


################
# Extract BLURBS
################

TIB <- actblue_js_full$js_rest
tcf <- TIB %>% select(title, contribution_blurb)

# We want to connect text of blurbs with entities
# ISSUE: There appear to be multiple buyers in some cases.
# SOLUTION: When there are multiple display names, just grab the first one:
display_name <- map(actblue_js_full$entities,"display_name") %>% map(1) 

# NB: Before unlisting, must  replace NULLs with NAs 
# [otherwise the vector will be too short for proper column-binding later]
display_name[sapply(display_name, is.null)] <- NA

display_name <- unlist(display_name)

# Now join the burbs with the candidate/PAC:
tcf <- cbind(tcf,display_name)

# Now unleash quanteda / start processing the texts:
corp <- corpus(tcf, text_field = "contribution_blurb")

toks <- corp %>% 
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

# How often do they use fighting words?
DFM[,c("fight")] %>% sum()
DFM[,c("defeat")] %>% sum()
DFM[,c("destroy")] %>% sum()
DFM[,c("destroying")] %>% sum()

# As of Apr. 23, 2021:
# 187,425 ads in the Actblue file/dataset
# 5,809,543 tokens
# 26,909 occurrences of the word “fight”
# 4,302 occurrences of the word “defeat”
# 374 occurrences of the word “destroy”

# To consider: compress rows by entities....
# corp_ent <- corpus(tcf, text_field = "contribution_blurb", groups="display_name")
# dfm_disp  <- DFM %>% dfm_group(groups = display_name)
# topfeatures(DFM,20)
# DFM_trimmed <- DFM %>% dfm_trim(min_termfreq = 120, verbose = FALSE)

# Make a simple cloud
set.seed(84104)
textplot_wordcloud(DFM_trimmed)

#########################
# MAKE A COPARISON CLOUD
#########################
corpus_subset(corp, 
              display_name %in% c("Nancy Pelosi", "Justice Democrats", "Raphael Warnock", "Bernie Sanders")) %>%
  tokens(remove_url = TRUE,
         remove_punct=TRUE) %>% 
  tokens_remove(stopwords("english")) %>% 
  tokens_remove(c("rt","amp","u8","<p>","<",">","div","img","alt","br",
                  "text-align","li","=","b","nbsp","style",
                  "em","p","strong",
                  "center","u",
                  "href","rel")) %>%
  tokens_tolower() %>%
  dfm() %>%
  dfm_group(groups = display_name) %>%
  dfm_trim(min_termfreq = 3, verbose = FALSE) %>%
  textplot_wordcloud(comparison = TRUE)
  


##############################################
# FREQUENCY of organizations / people [top 50]
##############################################
tcf %>% filter(!is.na(display_name)) %>% group_by(display_name) %>% tally() %>% 
  arrange(-n) %>% slice(1:50) %>%
    ggplot(aes(x = fct_reorder(display_name,n), y = n)) +
    geom_point() + 
    theme(axis.text.x = element_text(angle = 90, hjust = 1),
        text = element_text(size=14)) +
    labs(subtitle = "Top 50 orgs / people", x= "", y="") + 
    coord_flip()

####################
# FREQUENCY OF WORDS
####################
topF <- textstat_frequency(DFM, n = 75)

# Sort by reverse freq
topF$feature <- with(topF, reorder(feature, -frequency))

ggplot(topF, aes(x = feature, y = frequency)) +
  geom_point() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        text = element_text(size=14)) +
  labs(subtitle = "Top 75 features in the campaign blurbs")
