source(here::here("R", "utilities.R"))

# This script will be used to add firstname, gender, and firstname_prop_female
# to our datasets.

# Load data ====================================================================

load(here("data", "tidy", "fb_matched.Rda"))
load(here("data", "tidy", "fb_unique.Rda"))
load(here("data", "tidy", "fb_meta.Rda"))
#load(here("output", "fb_quanteda.Rda"))

## Not 100% sure how best to add the variable to fb_quanteda -- will take care
## of the others first, and ask about that one in Slack

# fb_matched ===================================================================

# As with other scripts, fb_matched is too large an object to push to GitHub --
# this section of the script will allow the generation of a new version for the
# local device

## Senate
# First, generate a firstname column
fb_matched$senate$firstname <- word(fb_matched$senate$candidate, 1)

# Next, because the gender package's gender function generates a dataframe,
# create this new data object
fbmatch_sen_gender <- gender(
  fb_matched$senate$firstname,
  years = c(1933, 1990), # 30 as of 2020, not 25
  method = c("ssa"),
  countries = c("United States")
)
# Isolate the variables we'll need -- name (for merging), gender (because that's
# what were here for, after all), and proportion_female (because the gender 
# assigned to some of these names will need to be double-checked in some cases -
# I believe Jamie Raskin, for example, will be coded as female here)
fbmatch_sen_gender <- as.data.frame(cbind(fbmatch_sen_gender$name, 
                                          fbmatch_sen_gender$gender,
                                          fbmatch_sen_gender$proportion_female))
colnames(fbmatch_sen_gender) <- c("firstname", "gender", 
                                  "firstname_prop_female")
# Now, we take out the duplicates here -- otherwise, the merge will fail
fbmatch_sen_gender <- unique(fbmatch_sen_gender)

# Merge variables into data
fb_matched$senate <- merge(fb_matched$senate, fbmatch_sen_gender, 
                           by = "firstname")

## House
# Repeat the Senate process for the House
fb_matched$house$firstname <- word(fb_matched$house$candidate, 1)

fbmatch_house_gender <- gender(
  fb_matched$house$firstname,
  years = c(1933, 1995), # 25 as of 2020
  method = c("ssa"),
  countries = c("United States")
)
fbmatch_house_gender <- as.data.frame(cbind(fbmatch_house_gender$name, 
                                            fbmatch_house_gender$gender,
                                        fbmatch_house_gender$proportion_female))
colnames(fbmatch_house_gender) <- c("firstname", "gender", 
                                    "firstname_prop_female")
fbmatch_house_gender <- unique(fbmatch_house_gender)

fb_matched$house <- merge(fb_matched$house, fbmatch_house_gender, 
                          by = "firstname")

# Saving updated dataset
save(fb_matched, file = here("data", "tidy", "fb_matched.Rda"))

# fb_unique ====================================================================

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
fbu_sen_gender <- as.data.frame(cbind(fbu_sen_gender$name, 
                                      fbu_sen_gender$gender,
                                      fbu_sen_gender$proportion_female))
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
fbu_house_gender <- as.data.frame(cbind(fbu_house_gender$name, 
                                        fbu_house_gender$gender,
                                        fbu_house_gender$proportion_female))
colnames(fbu_house_gender) <- c("firstname", "gender", "firstname_prop_female")
fbu_house_gender <- unique(fbu_house_gender)

fb_unique$house <- merge(fb_unique$house, fbu_house_gender, by = "firstname")

# Saving updated dataset
save(fb_unique, file = here("data", "tidy", "fb_unique.Rda"))

# fb_meta ======================================================================

## Senate
# First, generate a firstname column
fb_meta$senate$firstname <- word(fb_meta$senate$candidate, 1)

# Next, because the gender package's gender function generates a dataframe,
# create this new data object
fbmeta_sen_gender <- gender(
  fb_meta$senate$firstname,
  years = c(1933, 1990), # 30 as of 2020, not 25
  method = c("ssa"),
  countries = c("United States")
)
# Isolate the variables we'll need -- name (for merging), gender (because that's
# what were here for, after all), and proportion_female (because the genders 
# assigned to some of these names will need to be double-checked in some cases -
# Jaime Raskin, for example, will be coded as female here)
fbmeta_sen_gender <- as.data.frame(cbind(fbmeta_sen_gender$name, 
                                      fbmeta_sen_gender$gender,
                                      fbmeta_sen_gender$proportion_female))
colnames(fbmeta_sen_gender) <- c("firstname", "gender", "firstname_prop_female")
# Now, we take out the duplicates here -- otherwise, the merge will fail
fbmeta_sen_gender <- unique(fbmeta_sen_gender)

# Merge variables into data
fb_meta$senate <- merge(fb_meta$senate, fbmeta_sen_gender, by = "firstname")

## House
# Repeat the Senate process for the House
fb_meta$house$firstname <- word(fb_meta$house$candidate, 1)

fbmeta_house_gender <- gender(
  fb_meta$house$firstname,
  years = c(1933, 1995), # 25 as of 2020
  method = c("ssa"),
  countries = c("United States")
)
fbmeta_house_gender <- as.data.frame(cbind(fbmeta_house_gender$name, 
                                        fbmeta_house_gender$gender,
                                        fbmeta_house_gender$proportion_female))
colnames(fbmeta_house_gender) <- c("firstname", "gender", 
                                   "firstname_prop_female")
fbmeta_house_gender <- unique(fbmeta_house_gender)

fb_meta$house <- merge(fb_meta$house, fbmeta_house_gender, by = "firstname")

# Saving updated dataset
save(fb_meta, file = here("data", "tidy", "fb_meta.Rda"))

## fb_quanteda... ==============================================================

# Bringing the generative code from `10` -- will make some additions

fb_corpus <- fb_unique %>%
  imap(
    ~ corpus(
      .x$ad_creative_body,
      docvars = tibble(
        financial = .x$financial,
        candidate = .x$candidate,
        party = .x$party,
        vote_share = .x$vote_share,
        gender = .x$gender,
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

