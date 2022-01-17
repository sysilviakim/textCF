# Setup ========================================================================
source(here::here("R", "utilities.R"))
vec <- c(senate = "senate", house = "house")

# Import data ==================================================================
fb_list <- vec %>%
  map(
    function(x) {
      loadRData(paste0("data/raw/fb/fb-", x, "-raw-ads-2020.Rda")) %>%
        map_dfr("tbl", .id = "candidate")
    }
  ) %>%
  map(dedup)

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
# 188699 260933

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
      select(candidate, page_id, page_name, everything()) %>%
      mutate(vote_share = as.numeric(candidatevotes) / as.numeric(totalvotes))
  )

assert_that(nrow(fb_list$senate) == nrow(fb_matched$senate))
assert_that(nrow(fb_list$house) == nrow(fb_matched$house))
summary(fb_matched$senate$vote_share)
summary(fb_matched$house$vote_share)
assert_that(
  !any(fb_matched$senate$page_id != fb_matched$senate$fb_ad_library_id)
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
      )
  )

save(fb_matched, file = here("data", "tidy", "fb_matched.Rda"))
