source(here::here("R", "utilities.R"))

# Load data ====================================================================
## Text is in "ad_creative_body"
load(here("data", "tidy", "fb_matched.Rda"))
fb_matched$senate <- fb_matched$senate %>% rename(party = party_simplified)

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
        )
      ) %>%
      ungroup()
  )

fb_unique %>% map_dbl(nrow)
# senate  house
#  16828  43611

## Check nonclassified ad creative links
temp1 <- fb_unique$senate %>% filter(is.na(type))
View(sort(table(temp1$ad_creative_link_caption)))
temp2 <- fb_unique$house %>% filter(is.na(type))
View(sort(table(temp2$ad_creative_link_caption)))

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
