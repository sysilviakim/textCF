source(here::here("R", "utilities.R"))

# Load data ====================================================================
## Text is in "ad_creative_body"
load(here("data", "tidy", "fb_matched.Rda"))
load(here("data", "tidy", "fb_meta.Rda"))

## Some assertions for sanity checks
assert_that(unique_sanity_check(fb_matched$senate, "fb_ad_library_id") == 0)
assert_that(unique_sanity_check(fb_matched$house, "fb_ad_library_id") == 0)
assert_that(unique_sanity_check(fb_matched$senate, "party") == 0)
assert_that(unique_sanity_check(fb_matched$house, "party") == 0)
assert_that(unique_sanity_check(fb_matched$senate, "inc") == 0)
assert_that(unique_sanity_check(fb_matched$house, "inc") == 0)
assert_that(unique_sanity_check(fb_matched$senate, "pvi") == 0)
assert_that(unique_sanity_check(fb_matched$house, "pvi") == 0)

# Deduplicated, simplified ad data =============================================
fb_unique <- fb_matched %>%
  map(
    ~ .x %>%
      select(
        candidate, fb_ad_library_id, page_name, party, inc, state_po, pvi, 
        gender, proportion_female, contains("state_cd"), 
        ad_creative_body, ad_creative_link_caption, vote_share
      ) %>%
      mutate(
        ## If it is merely two spaces as opposed to one, unify
        ad_creative_body = trimws(gsub("\\s+", " ", ad_creative_body))
      ) %>%
      distinct(candidate, ad_creative_body, .keep_all = TRUE) %>%
      ## No need to have distinct `ad_creative_link_caption` or `page_name`
      filter(ad_creative_body != "") %>%
      filter(ad_creative_body != "{{product.brand}}") %>%
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
          is.na(ad_creative_link_caption) ~ "Voter-targeting",
          # grepl(" ", ad_creative_link_caption) ~ "Voter-targeting",
          grepl(
            "conversation with |town hall|meet |tour stop |iwillvote.com",
            ad_creative_link_caption
          ) ~ "Voter-targeting",
          grepl(".gov", ad_creative_link_caption) ~ "Government Information",
          grepl("secure.|act.|action.|go.", ad_creative_link_caption) ~
          "Personal Contribution Link",
          grepl("facebook.com|fb.me", ad_creative_link_caption) ~
          "Facebook Page",
        )
      ) %>%
      donate_classify() %>%
      ungroup()
  )

fb_unique %>% map_dbl(nrow)
# senate  house
#  24860  40375

## assertions? 
## Can't, because Mike Lee and Ted Cruz had a joint campaign, same text
## Similarly, some cross-candidate overlapping text
## That's okay, because we want to see *within* candidate variation
fb_unique$senate %>% group_by(ad_creative_body) %>% filter(n() > 1) %>% View()
fb_unique$house %>% group_by(ad_creative_body) %>% filter(n() > 1) %>% View()

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
          ## Those that are donate == TRUE but type is NA or Voter-targeting
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
        )
      ) %>%
      ungroup()
  )

prop(fb_unique$senate, vars = "word_trump") ## 15.4%
prop(fb_unique$house, vars = "word_trump") ## 15.8%

# Paste meta data ==============================================================
fb_unique_raw <- fb_unique
save(fb_unique_raw, file = here("data", "tidy", "fb_unique_raw.Rda"))

fb_unique <- c(senate = "senate", house = "house") %>%
  map(
    function(x) {
      left_join(clean_candidate(fb_unique[[x]]), fb_meta[[x]]) %>%
        mutate(n = row_number()) %>%
        group_by(n) %>%
        group_split() %>%
        map_dfr(
          ~ {
            if (!is.na(.x$state_po) & (.x$state_po %in% state.abb)) {
              .x %>%
                mutate(
                  ## Otherwise, sticks with the first value of the dataframe
                  in_district :=
                    !!as.name(paste0("mean_", tolower(.x$state_po))),
                  in_district = case_when(
                    is.nan(in_district) ~ NA_real_,
                    TRUE ~ in_district
                  ),
                  out_district = 1 - in_district
                )
            } else {
              .x
            }
          }
        )
    }
  ) %>%
  map(~ .x %>% select(candidate, party, financial, in_district, everything()))
assert_that(nrow(fb_unique_raw$senate) == nrow(fb_unique$senate))
assert_that(nrow(fb_unique_raw$house) == nrow(fb_unique$house))

## Some verification that "Voter-targeting" are targeting in-district
temp <- fb_unique %>%
  map_dfr(
    ~ .x %>%
      filter(!is.na(party) & party != "INDEPENDENT") %>%
      group_by(party, financial) %>%
      summarise(
        mean = mean(in_district, na.rm = TRUE),
        sd = sd(in_district, na.rm = TRUE),
        se = sd(in_district, na.rm = TRUE) / sqrt(n())
      ),
    .id = "chamber"
  ) %>%
  rowwise() %>%
  mutate(
    chamber = simple_cap(tolower(chamber)),
    party = simple_cap(tolower(party)),
    financial = simple_cap(tolower(financial)),
    Party = glue("{party},\n{financial}")
  ) %>%
  ungroup() %>%
  select(Party, everything())

temp %>%
  pivot_wider(
    id_cols = c("party", "chamber"),
    names_from = "financial", values_from = c("mean", "se")
  )

pdf(here("fig", "in_district_per_type_chamber.pdf"), width = 6, height = 2.8)
print(
  fb_mention_plot(temp, xvar = "mean", se = "se", xlab = "") +
    scale_x_continuous(limits = c(0, 1))
)
dev.off()

fb_unique %>%
  map_dbl(
    ~ .x %>%
      filter(financial == "Voter-targeting" & in_district < 0.5) %>%
      nrow()
  )

# Saving fb_unique =============================================================
save(fb_unique, file = here("data", "tidy", "fb_unique.Rda"))

# Building a corpus ============================================================
fb_corpus <- fb_unique %>%
  imap(
    ~ corpus(
      .x$ad_creative_body,
      docvars = tibble(
        financial = .x$financial,
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
corp_FB <- fb_corpus$senate + fb_corpus$house

# Tokenize documents ===========================================================
toks_FB <- tokens(corp_FB) %>%
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
  tokens_remove(setdiff(removing_tokens, c("strong", "center"))) %>%
  tokens_remove(letters)

# Each document will be an ad ==================================================
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
