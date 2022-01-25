source(here::here("R", "utilities.R"))

# Load data ====================================================================
## Text is in "ad_creative_body"
load(here("data", "tidy", "fb_matched.Rda"))

# Deduplicated, simplified ad data =============================================
fb_unique <- fb_matched %>%
  map(
    ~ .x %>%
      select(
        candidate,
        fb_ad_library_id, page_name, party, inc, state_po, contains("state_cd"),
        ad_creative_body, ad_creative_link_caption, vote_share
      ) %>%
      distinct() %>%
      filter(ad_creative_body != "") %>%
      ## Antonio Delgado duplicates appear (House, with wrong PID)
      ## Drop these rows
      filter(!(page_name == "Antonio Delgado" & party == "REPUBLICAN")) %>%
      ## Label ad types by conduit/platform
      mutate(ad_creative_link_caption = tolower(ad_creative_link_caption)) %>%
      rowwise() %>%
      ## Perform twice to unify to similar patterns
      simplify_ad_body() %>%
      simplify_ad_body() %>%
      ## Classify [ongoing!]
      mutate(
        ## Is the typical platform links in the caption link?
        type = case_when(
          grepl("actblue.com", ad_creative_link_caption) ~ "ActBlue",
          grepl("winred.com", ad_creative_link_caption) ~ "WinRed",
          grepl("ngpvan.com|myngp.com", ad_creative_link_caption) ~ "NGP VAN",
          grepl("anedot.com", ad_creative_link_caption) ~ "Anedot",
          grepl("victorypassport.com", ad_creative_link_caption) ~
          "Victory Passport",
          grepl("fundraiser", ad_creative_link_caption) ~ "Misc.",
          is.na(ad_creative_link_caption) ~ "Non-financial",
          # grepl(" ", ad_creative_link_caption) ~ "Non-financial",
          grepl(
            "conversation with |town hall|meet |tour stop |iwillvote.com",
            ad_creative_link_caption
          ) ~ "Non-financial"
        ),
        ## Or does the text contain references to donations?
        donate = case_when(
          grepl(
            paste0(
              "donate|donation|contribute|contribution|chip in|pitch in|",
              "PAC money|corporate PAC|dollar| bucks|\\$|",
              "end-of-month deadline|end-of-quarter deadline|",
              "end-of-year deadline|match opportunity|match fund"
            ),
            tolower(ad_creative_body)
          ) ~ TRUE,
          TRUE ~ FALSE
        )
      ) %>%
      ungroup()
  )

fb_unique %>% map_dbl(nrow)
# senate  house
#  16828  43611

## Check nonclassified ad creative links
# temp1 <- fb_unique$senate %>% filter(is.na(type))
# View(sort(table(temp1$ad_creative_link_caption)))
# temp2 <- fb_unique$house %>% filter(is.na(type))
# View(sort(table(temp2$ad_creative_link_caption)))

## Compare with ad_creative_body keyword
table(fb_unique$senate$type, fb_unique$senate$donate, useNA = "ifany")
table(fb_unique$house$type, fb_unique$house$donate, useNA = "ifany")
temp <- fb_unique %>%
  map(
    ~ .x %>%
      filter(
        !(
          ## The only instances that the classification is double-verified
          donate == TRUE &
            type %in% c(
              "ActBlue", "Anedot", "Misc.", 
              "NGP VAN", "Victory Passport", "WinRed"
            )
          ## Those that are donate == TRUE but type is NA or non-financial
          ## Some of it is that it is targeting both types of electorates
        )
      ) %>%
      select(ad_creative_body, donate, type, everything())
  )

# Create keyword columns: Trump and COVID ======================================
fb_unique <- fb_unique %>%
  map(
    ~ .x %>%
      rowwise() %>%
      mutate(
        word_trump = case_when(
          str_detect(str_to_lower(ad_creative_body), "trump") ~ 1,
          !is.na(ad_creative_body) ~ 0
        ),
        word_covid = case_when(
          str_detect(
            str_to_lower(ad_creative_body),
            paste(
              c(
                "covid", "#covid", "covid-19", "#covid19",
                "coronavirus", "virus",
                "infection", "infected",
                "vaccine", "vaccines"
              ),
              collapse = "|"
            )
          ) ~ 1,
          !is.na(ad_creative_body) ~ 0
        ),
        word_chinese = case_when(
          str_detect(str_to_lower(ad_creative_body), "chinese|china") ~ 1,
          !is.na(ad_creative_body) ~ 0
        )
      ) %>%
      ungroup()
  )

prop(fb_unique$senate, vars = "word_trump") ## 16.6%
prop(fb_unique$house, vars = "word_trump") ## 15.7%
prop(fb_unique$senate, vars = "word_covid") ## 3.0%
prop(fb_unique$house, vars = "word_covid") ## 4.4%
prop(fb_unique$senate, vars = "word_chinese") ## 1.5%
prop(fb_unique$house, vars = "word_chinese") ## 0.8%

save(fb_unique, file = here("data", "tidy", "fb_unique.Rda"))

# Building a corpus ============================================================
fb_corpus <- fb_unique %>%
  imap(
    ~ corpus(
      .x$ad_creative_body,
      docvars = tibble(
        type = .x$type,
        candidate = .x$candidate,
        party = .x$party,
        vote_share = .x$vote_share,
        chamber = .y
      )
    )
  )

## Rename the documents, adding by-chamber prefix:
docnames(fb_corpus$house) <- paste0("house_", 1:ndoc(fb_corpus$house))
docnames(fb_corpus$senate) <- paste0("senate_", 1:ndoc(fb_corpus$senate))

## Combine two chambers
CORP_FB <- fb_corpus$senate + fb_corpus$house

# TOKENIZE DOCUMENTS ===========================================================
toks_FB <- tokens(CORP_FB) %>%
  tokens(
    remove_url = TRUE,
    remove_punct = TRUE,
    include_docvars = TRUE
  ) %>%
  # tokens_wordstem() %>%
  tokens_tolower() %>%
  tokens_remove(stopwords("english")) %>%
  tokens_remove(stopwords("spanish")) %>%
  # tokens_remove(c("rt", "amp", "u8")) %>%
  tokens_remove(setdiff(removing_tokens, c("strong", "center")))

# Each document will be an AD ==================================================
dfm_FB_ad <- dfm(toks_FB)
dfm_FB_ad_prop <- dfm_weight(dfm_FB_ad, scheme = "prop")

# Each document will be a candidate ============================================
dfm_FB_cand <- dfm_group(dfm_FB_ad, groups = candidate)
dfm_FB_cand_prop <- dfm_weight(dfm_FB_cand, scheme = "prop")

# Save for import ==============================================================
save(
  fb_corpus, toks_FB, dfm_FB_ad, dfm_FB_ad_prop, dfm_FB_cand, dfm_FB_cand_prop,
  file = here("output", "fb_quanteda.Rda")
)
