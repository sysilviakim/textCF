source(here::here("R", "utilities.R"))

# Load data ====================================================================
## Text is in "ad_creative_body"
load(here("data", "tidy", "fb_matched.Rda"))
load(here("data", "tidy", "fb_meta.Rda"))

# Deduplicated, simplified ad data =============================================
fb_unique <- fb_matched %>%
  map(
    ~ .x %>%
      select(
        candidate,
        fb_ad_library_id, page_name, party, inc, state_po, pvi,
        contains("state_cd"), ad_creative_body, ad_creative_link_caption,
        vote_share ## ,
        ## contains("ad_"), contains("spend_"), contains("potential_"),
        ## contains("impressions_"),
        ## matches(
        ##   paste0("^", tolower(state.abb) %>% paste(collapse = "$|^"), "$")
        ## ),
        ## contains("male_"), contains("female_"), contains("unknown_"),
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
#  26113  43949

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
        ),
        word_biden = case_when(
          str_detect(str_to_lower(ad_creative_body), "biden") ~ 1,
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
        ## Ended up not using for the first project
        word_chinese = case_when(
          str_detect(str_to_lower(ad_creative_body), "chinese|china") ~ 1,
          !is.na(ad_creative_body) ~ 0
        )
      ) %>%
      ungroup()
  )

prop(fb_unique$senate, vars = "word_trump") ## 16.6%
prop(fb_unique$house, vars = "word_trump") ## 15.7%
prop(fb_unique$senate, vars = "word_biden") ## 1.0%
prop(fb_unique$house, vars = "word_biden") ## 0.9%
prop(fb_unique$senate, vars = "word_covid") ## 3.0%
prop(fb_unique$house, vars = "word_covid") ## 4.4%

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

# Adding gender variable to fb_unique ==========================================

## Senate
# First, generate a firstname column
fb_unique$senate$firstname <- word(fb_unique$senate$candidate, 1)

# Next, because the gender package's gender function generates a dataframe,
# create this new data object
fbu_sen_gender <- gender(
  fb_unique$senate$firstname,
  years = c(1933, 1990), # 30 as of 2020, not 25
  method = c("ssa"),
  countries = c("United States")
)
# Isolate the variables we'll need -- name (for merging), gender (because that's
# what were here for, after all), and proportion_female (because the genders
# assigned to some of these names will need to be double-checked in some cases -
# Jaime Raskin, for example, will be coded as female here)
fbu_sen_gender <- as.data.frame(cbind(
  fbu_sen_gender$name,
  fbu_sen_gender$gender,
  fbu_sen_gender$proportion_female
))
colnames(fbu_sen_gender) <- c("firstname", "gender", "firstname_prop_female")
# Now, we take out the duplicates here -- otherwise, the merge will fail
fbu_sen_gender <- unique(fbu_sen_gender)

# Merge variables into data
fb_unique$senate <- merge(fb_unique$senate, fbu_sen_gender, by = "firstname")

## House
# Repeat the Senate process for the House
fb_unique$house$firstname <- word(fb_unique$house$candidate, 1)

fbu_house_gender <- gender(
  fb_unique$house$firstname,
  years = c(1933, 1995), # 25 as of 2020
  method = c("ssa"),
  countries = c("United States")
)
fbu_house_gender <- as.data.frame(
  cbind(
    fbu_house_gender$name,
    fbu_house_gender$gender,
    fbu_house_gender$proportion_female
  )
)
colnames(fbu_house_gender) <- c("firstname", "gender", "firstname_prop_female")
fbu_house_gender <- unique(fbu_house_gender)

fb_unique$house <- merge(fb_unique$house, fbu_house_gender, by = "firstname")

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
