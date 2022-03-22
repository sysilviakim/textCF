# Setup ========================================================================
source(here::here("R", "utilities.R"))
vec <- c(senate = "senate", house = "house")

# Import data ==================================================================
fb_list <- vec %>%
  map(~ loadRData(here("data", "tidy", paste0("fb_", .x, "_merged.Rda")))) %>%
  map(~ .x %>% rename(adlib_id = id))

# Candidate list ===============================================================
cand_list <- vec %>%
  map(
    ~ read_csv(
      here("data", "raw", "fb", paste0("fb-", .x, ".csv")),
      col_types = cols(.default = "c")
    ) %>%
      select(where(~ !all(is.na(.x))))
  ) %>%
  map(dedup)

cong_complete <- loadRData("data/tidy/cong_complete.Rda") %>%
  map(
    ~ .x %>%
      clean_names() %>%
      select(
        -year, -contains("stage"), -contains("special"),
        -contains("candidatevotes"), -contains("totalvotes"), -contains("party")
      ) %>%
      select(candidate, everything())
  ) %>%
  ## dedup here is insufficient, because sometimes, there are multiple
  ## display_name_brandings, id_brandings, default, image_url, mobile_image_url
  map(
    ~ .x %>%
      group_by(candidate) %>%
      filter(!(n() > 1 & is.na(header_url))) %>%
      ungroup() %>%
      select(-display_name_brandings, -id_brandings, -default, -image_url) %>%
      select(
        -contains("mobile_image"), -contains("background_image"),
        -contains("header_image"), -contains("image_href"),
        -contains("arrangement"), -contains("public_"), -contains("file_"),
        -contains("shared_title"), -contains("shared_contribution_blurb"),
        -contains("full_background"), -contains("reusable")
      ) %>%
      dedup()
  )

assert_that(!any(duplicated(cong_complete$senate$candidate)))
assert_that(!any(duplicated(cong_complete$house$candidate)))

fb_list %>% map_dbl(nrow)
# senate  house
# 344536 260934

cand_list %>% map_dbl(nrow)
# senate  house
#     75    868

cong_complete %>% map_dbl(nrow)
# senate  house
#    137    781

# Match candidate-level characteristics ========================================
## FB original data
fb_matched <- vec %>%
  map(
    ~ cong_complete[[.x]] %>%
      ## Joining, by = "candidate"
      left_join(
        left_join(fb_list[[.x]], cand_list[[.x]]) %>%
          rename(state_name = state) %>%
          mutate(candidate = trimws(gsub('"', "", candidate))),
        .
      ) %>%
      select(candidate, page_id, page_name, inc, everything()) %>%
      mutate(vote_share = as.numeric(candidatevotes) / as.numeric(totalvotes))
  )

##### Trying to figure out why we're getting NAs

## So, let's establish what we know. Senate as example, because less to search.
match_sen_list <- as.data.frame(cbind(fb_matched[['senate']]$candidate,
                                      fb_matched[['senate']]$inc))
match_sen_list <- unique(match_sen_list)
cc_sen_list <- as.data.frame(cbind(cong_complete[['senate']]$candidate,
                                   cong_complete[['senate']]$inc))
mismatches <- rbind(match_sen_list,cc_sen_list)
# So, 248 in total...let's see how many of these aren't exact duplicates 
mismatches <- mismatches[!(duplicated(mismatches) | duplicated(mismatches, 
                                                 fromLast = TRUE)), ]
# 118 unique name-incumbency pairings
## not actually unique names (necessarily), but ones that R thinks are unique

# mismatches contains all the candidate-inc pairings that do not appear in
# exactly the same manner in fb_matched[['senate]] and cong_complete[['senate]].
## Some of the names here are folks who didn't have ad library pages at all,
## so that makes sense. These are many of the ones that appear in all-caps and
## have an inc variable, but don't have a corresponding pair.
## Some of them (those without incumbency and without a corresponding name that
## features the inc variable) do not appear in cong_complete, i.e. Alex Padilla
## And some of them feature middle initials and/or real names in one, and not in
## not in the other -- i.e. CHARLES E. SCHUMER and Chuck Schumer

## What I can't figure out are the ones like Amy Klobuchar and Bernie Sanders...
## What happened with them? Their names appear as they should in cong_complete.
## Why are they getting messed up?

## Where are these lowercase names coming from? fb_list, it seems. Let's try...
fb_list[['senate']]$candidate <- toupper(fb_list[['senate']]$candidate)
# To see if this worked...
#test <- as.data.frame(unique(fb_list[['senate']]$candidate))
# yep, it worked...if we redo this step, will it at least sort out these folks?

fb_matched <- vec %>%
  map(
    ~ cong_complete[[.x]] %>%
      ## Joining, by = "candidate"
      left_join(
        left_join(fb_list[[.x]], cand_list[[.x]]) %>%
          rename(state_name = state) %>%
          mutate(candidate = trimws(gsub('"', "", candidate))),
        .
      ) %>%
      select(candidate, page_id, page_name, inc, everything()) %>%
      mutate(vote_share = as.numeric(candidatevotes) / as.numeric(totalvotes))
  )
match_sen_list <- as.data.frame(cbind(fb_matched[['senate']]$candidate,
                                      fb_matched[['senate']]$inc))
match_sen_list <- unique(match_sen_list)
cc_sen_list <- as.data.frame(cbind(cong_complete[['senate']]$candidate,
                                   cong_complete[['senate']]$inc))
mismatches <- rbind(match_sen_list,cc_sen_list)
mismatches <- mismatches[!(duplicated(mismatches) | duplicated(mismatches, 
                                                            fromLast = TRUE)), ]
# This has worked for a significant number of these candidates --
# where there were 118 mismatches, now there are 68.
# Will do the same for the House -- hopefully will alleviate things there
fb_list[['house']]$candidate <- toupper(fb_list[['house']]$candidate)
# Before we rerun, let's see about the House side of things...
match_hr_list <- as.data.frame(cbind(fb_matched[['house']]$candidate,
                                      fb_matched[['house']]$inc))
match_hr_list <- unique(match_hr_list)
cc_hr_list <- as.data.frame(cbind(cong_complete[['house']]$candidate,
                                   cong_complete[['house']]$inc))
mismatches_hr <- rbind(match_hr_list,cc_hr_list)
mismatches_hr <- mismatches_hr[!(duplicated(mismatches_hr) | 
                                   duplicated(mismatches_hr, 
                                              fromLast = TRUE)), ]
# 281 mismatches...

fb_matched <- vec %>%
  map(
    ~ cong_complete[[.x]] %>%
      ## Joining, by = "candidate"
      left_join(
        left_join(fb_list[[.x]], cand_list[[.x]]) %>%
          rename(state_name = state) %>%
          mutate(candidate = trimws(gsub('"', "", candidate))),
        .
      ) %>%
      select(candidate, page_id, page_name, inc, everything()) %>%
      mutate(vote_share = as.numeric(candidatevotes) / as.numeric(totalvotes))
  )
match_hr_list <- as.data.frame(cbind(fb_matched[['house']]$candidate,
                                     fb_matched[['house']]$inc))
match_hr_list <- unique(match_hr_list)
cc_hr_list <- as.data.frame(cbind(cong_complete[['house']]$candidate,
                                  cong_complete[['house']]$inc))
mismatches_hr <- rbind(match_hr_list,cc_hr_list)
mismatches_hr <- mismatches_hr[!(duplicated(mismatches_hr) | 
                                   duplicated(mismatches_hr, 
                                              fromLast = TRUE)), ]
# Still 281...


##### Back to Match candidate-level characteristics

assert_that(nrow(fb_list$senate) == nrow(fb_matched$senate))
assert_that(nrow(fb_list$house) == nrow(fb_matched$house))
summary(fb_matched$senate$vote_share)
summary(fb_matched$house$vote_share)
assert_that(
  !any(
    fb_matched$senate$page_id != fb_matched$senate$fb_ad_library_id,
    na.rm = TRUE ## Newly added candidates who didn't run in 2020
  )
  ## Problem with things like 1.58529E+15
)

fb_matched <- fb_matched %>%
  map(
    ~ .x %>%
      select(-fb_ad_library_id) %>%
      select(fb_ad_library_id = page_id, everything())
  )

# Organize party variable ======================================================
fb_matched$senate <- fb_matched$senate %>% rename(party = party_simplified)
fb_matched <- fb_matched %>%
  map(
    ~ .x %>%
      mutate(
        party = case_when(
          ## DEMOCRATIC-FARMER-LABOR and also DEMOCRATIC-NONPARTISAN LEAGUE
          grepl("DEMOCRAT", party) ~ "DEMOCRAT",
          TRUE ~ party
        )
      ) %>%
      rename(id_old = id) %>%
      rename(id = id_2)
  )

save(fb_matched, file = here("data", "tidy", "fb_matched.Rda"))

# Generate and store metadata for ads, before taking out unique ads ============
fb_simple <- fb_matched %>%
  map(
    ~ .x %>%
      group_by(candidate, ad_creative_body) %>%
      select(
        matches(
          ## Excluding NA values for demo/region target
          paste0(
            paste0(
              "unknown|male|impression|potential|spend|ad_creation_date|",
              "delivery|"
            ),
            "^", tolower(state.abb) %>% paste(collapse = "$|^"), "$"
          )
        )
      )
  )
assert_that(length(setdiff(tolower(state.abb), names(fb_simple$senate))) == 0)

## The list(mean, sd), .names = "{.col}.fn{.fn}" approach didn't work;
## Not sure why; but since I also need na.rm = TRUE, just use the agg. approach
fb_meta <- fb_simple %>%
  map(
    ~ {
      out_mean <- .x %>% summarise(
        across(
          everything(), function(x) mean(as.numeric(x), na.rm = TRUE),
          .names = "mean_{.col}"
        )
      )
      out_min <- .x %>% summarise(
        across(
          everything(), function(x) min(as.numeric(x), na.rm = TRUE),
          .names = "min_{.col}"
        )
      )
      out_max <- .x %>% summarise(
        across(
          everything(), function(x) max(as.numeric(x), na.rm = TRUE),
          .names = "max_{.col}"
        )
      )
      out_sd <- .x %>% summarise(
        across(
          everything(), function(x) sd(as.numeric(x), na.rm = TRUE),
          .names = "sd_{.col}"
        )
      )
      return(
        left_join(left_join(out_mean, out_sd), left_join(out_min, out_max))
      )
    }
  )

## Make sure to NA the NaN values
## Make sure to as.Date(x, origin = "1970-01-01") for wrangled dates
save(fb_meta, file = here("data", "tidy", "fb_meta.Rda"))
