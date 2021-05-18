library(tidyverse)
library(quanteda)
library(quanteda.textplots)
library(quanteda.textstats)
# https://muellerstefan.net/files/quanteda-cheatsheet.pdf

load("data/raw/2022/actblue/actblue_js_scraped.Rda")
load("data/raw/2022/winred/winred_text_scraped.Rda")

#load("data/raw/2022/house/actblue_js_scraped.Rda")
#load("data/raw/2022/house/anedot_text_scraped.Rda")
#load("data/raw/2022/house/winred_text_scraped.Rda")

names(actblue_js_full)
names(winred_text)


act_blurb <- actblue_js_full$js_rest$contribution_blurb

# Clear some memory
rm(actblue_js_full)


# Get text from ActBlue
c1 <- corpus(act_blurb)

# Get text and other data from WinRed
c2 <- corpus(winred_text %>% select(description,, 
                                    short_text = text, 
                                    name, 
                                    og_description),
                                    text_field = "description")

c1$platform <- "ActBlue"
c2$platform <- "WinRed"

# Documents also need distinct names
docnames(c2) <- paste0("winred",seq(1:length(c2)))

corp <- c1 + c2



