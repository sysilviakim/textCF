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

cong_complete <- loadRData(here("data/tidy/cong_complete.Rda")) %>%
  map(
    ~ .x %>%
      clean_names() %>%
      select(
        -year, -contains("stage"), -contains("special"),
        -contains("candidatevotes"), -contains("totalvotes"),
        -contains("party")
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
      dedup() %>%
      ## use bind_rows(., id = "chamber") later; some missing values here
      select(-chamber)
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
#    144    817

# Creating initial fb_matched object ===========================================

## Sanity checks
assert_that(!any(duplicated(cand_list$senate$candidate)))
assert_that(!any(duplicated(cand_list$house$candidate)))

## FB original data
fb_matched <- vec %>%
  map(
    ~ clean_candidate(cong_complete[[.x]]) %>%
      mutate(
        candidate = case_when(
          ## final manual changes
          candidate == "tom halleran" ~ "tom ohalleran",
          grepl(" mueri", candidate) ~ "hillary oconnor mueri",
          candidate == "christopher murphy" ~ "chris murphy",
          candidate == "robert casey jr" ~ "bob casey",
          candidate == "charles schumer" ~ "chuck schumer",
          TRUE ~ candidate
        )
      ) %>%
      ## Joining, by = "candidate": full candidate characteristics
      left_join(
        ## Joining, by = "candidate": allowing fb_ad_library_id to be matched
        left_join(
          ## To ensure candidates such as Grijalva getting matched
          ## cleaned up the .CSV file's names
          clean_candidate(fb_list[[.x]]),
          clean_candidate(cand_list[[.x]])
        ) %>%
          rename(state_name = state) %>%
          mutate(candidate = trimws(gsub('"', "", candidate))),
        .
      ) %>%
      select(candidate, page_id, page_name, inc, everything()) %>%
      mutate(vote_share = as.numeric(candidatevotes) / as.numeric(totalvotes))
  )

## Sanity checks
fb_matched$house %>%
  filter(candidate == "cori bush") %>%
  .$inc %>%
  table(useNA = "ifany")

fb_matched$house %>%
  filter(candidate == "cori bush") %>%
  .$pvi %>%
  table(useNA = "ifany")

fb_matched$house %>%
  filter(candidate == "kevin mccarthy") %>%
  .$pvi %>%
  table(useNA = "ifany")

fb_matched$house %>%
  filter(candidate == "ilhan omar") %>%
  nrow()

sum(is.na(fb_matched$senate$inc)) ## 147,196 ---> 152
sum(is.na(fb_matched$house$inc)) ## 9,933 ---> 4,737
sum(is.na(fb_matched$senate$pvi)) ## 152
sum(is.na(fb_matched$house$pvi)) ## 4,737

# Identifying which candidates still have NAs ==================================
## Need manual case_when fill-ins
fb_matched %>%
  map(
    ~ .x %>%
      filter(is.na(inc)) %>%
      .$candidate %>%
      unique()
  )

# $senate
# [1] "alex padilla" ---> was not running for Senate in 2020, appointed
# [2] "rand paul"
# [3] "pat toomey"
#
# $house
# [1] "mark razzoli"
# [2] "diane mitsch bush"
# [3] "tim kelly"
# [4] "david torres"
# [5] "kevin van ausdal"
# [6] "john briscoe"
# [7] "zach raknerud"
# [8] "tawnja zahradka"
# [9] "quinn nystrom"
# [10] "christy smith"
# [11] "david scott"
# [12] "michael san nicolas"
# [13] "randy weber sr"
# [14] "frank lucas"
# [15] "c antonio delgado"
# [16] "tracy jennings"
# [17] "ricardo rick de la fuente"
# [18] "ben gibson"
# [19] "dan feehan"
# [20] "liz johnson"

# Manual fixes =================================================================

## Senate Incumbency
fb_matched$senate <- fb_matched$senate %>%
  mutate(
    inc = case_when(
      candidate == "pat toomey" ~ "INCUMBENT",
      candidate == "rand paul" ~ "INCUMBENT",
      TRUE ~ inc
    )
  )

## Removing Alex Padilla, because he wasn't running for Senate
fb_matched$senate <- fb_matched$senate[
  !(fb_matched$senate$candidate == "alex padilla"),
]

## House Incumbency
fb_matched$house <- fb_matched$house %>%
  mutate(
    inc = case_when(
      candidate == "mark razzoli" ~ "CHALLENGER",
      candidate == "diane mitsch bush" ~ "OPEN",
      candidate == "tim kelly" ~ "CHALLENGER",
      candidate == "david torres" ~ "CHALLENGER",
      candidate == "kevin van ausdal" ~ "OPEN",
      candidate == "john briscoe" ~ "CHALLENGER",
      candidate == "zach raknerud" ~ "CHALLENGER",
      candidate == "tawnja zahradka" ~ "CHALLENGER",
      candidate == "quinn nystrom" ~ "CHALLENGER",
      candidate == "christy smith" ~ "CHALLENGER",
      candidate == "david scott" ~ "INCUMBENT",
      candidate == "michael san nicolas" ~ "INCUMBENT",
      candidate == "randy weber sr" ~ "INCUMBENT",
      candidate == "frank lucas" ~ "INCUMBENT",
      candidate == "c antonio delgado" ~ "CHALLENGER",
      candidate == "tracy jennings" ~ "CHALLENGER",
      candidate == "ricardo rick de la fuente" ~ "CHALLENGER",
      candidate == "ben gibson" ~ "CHALLENGER",
      candidate == "dan feehan" ~ "CHALLENGER",
      candidate == "liz johnson" ~ "CHALLENGER",
      TRUE ~ inc
    )
  )

sum(is.na(fb_matched$senate$inc)) ## 152 ---> 0
sum(is.na(fb_matched$house$inc)) ## 4,737 ---> 0

## Senate PVI

class(fb_matched$house$pvi)
class(fb_matched$senate$pvi)
# Numeric

fb_matched$senate <- fb_matched$senate %>%
  mutate(
    pvi = case_when(
      candidate == "pat toomey" ~ -2,
      candidate == "rand paul" ~ -18,
      TRUE ~ pvi
    )
  )

sum(is.na(fb_matched$senate$pvi)) ## 115 ---> 0 -- success!

## House PVI

fb_matched$house <- fb_matched$house %>%
  mutate(
    pvi = case_when(
      candidate == "mark razzoli" ~ 16, # NJ-12; D+16
      candidate == "diane mitsch bush" ~ -6, # CO-3; R+6
      candidate == "tim kelly" ~ 5, # MI-5; D+5
      candidate == "david torres" ~ 40, # PA-2; D+40
      candidate == "kevin van ausdal" ~ -27, # GA-14; R+27
      candidate == "john briscoe" ~ 13, # CA-47; D+13
      candidate == "zach raknerud" ~ -16, # ND-AL; R+16
      candidate == "tawnja zahradka" ~ -12, # MN-6; R+12
      candidate == "quinn nystrom" ~ -4, # MN-8 R+4
      candidate == "christy smith" ~ 0, # CA-25; EVEN
      candidate == "david scott" ~ 20, # GA-13; D+20
      # candidate == "michael san nicolas" ~ ___, # Guam... should prob. remove
      candidate == "randy weber sr" ~ -12, # TX-14; R+12
      candidate == "frank lucas" ~ -27, # OK-3; R+27
      candidate == "c antonio delgado" ~ 33, # CA-40; D+33
      candidate == "tracy jennings" ~ 38, # IL-7; D+38
      candidate == "ricardo rick de la fuente" ~ -13, # TX-27; R+13
      candidate == "ben gibson" ~ -13, # LA-4; R+13
      candidate == "dan feehan" ~ -5, # MN-1; R+5
      candidate == "liz johnson" ~ -9, # GA-12; R+9
      TRUE ~ pvi
    )
  )

### Removing Michael San Nicolas, because he represents Guam (sorry, Guam)
fb_matched$house <- fb_matched$house[
  !(fb_matched$house$candidate == "michael san nicolas"),
]
sum(is.na(fb_matched$house$pvi)) ## 4,737 ---> 0

# Match candidate-level characteristics ========================================

# assert_that(nrow(fb_list$senate) == nrow(fb_matched$senate))
# assert_that(nrow(fb_list$house) == nrow(fb_matched$house))
# These now give errors, but this is because we removed Alex Padilla's rows
# from fb_matched$ senate, and Michael San Nicolas's from fb_matched$house.

summary(fb_matched$senate$vote_share)
summary(fb_matched$house$vote_share)
assert_that(
  !any(
    fb_matched$senate$page_id != fb_matched$senate$fb_ad_library_id,
    na.rm = TRUE ## Newly added candidates who didn't run in 2020
  )
  ## Problem with things like 1.58529E+15
)
# TRUE

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

# Adding gender variable =======================================================
fb_matched_old <- fb_matched
fb_matched <- fb_matched %>%
  map(
    ~ .x %>%
      ## so that if w gregory steube, we get "gregory" not "w"
      mutate(
        first_name = word(
          gsub(
            letters %>% paste(collapse = " |^") %>%
              paste0("^", ., " "), "", candidate
          ),
          1
        )
      ) %>%
      gender_mutate_df(., input_name = "first_name", years = c(1933, 1990))
  )

assert_that(!any(is.na(fb_matched$senate$gender)))
fb_matched$house %>%
  filter(is.na(gender)) %>%
  .$candidate %>%
  table()

#       albio sires              andr carson            g butterfield 
#                12                       18                       27 
#        ilhan omar               j scholten             jahana hayes 
#              2054                      123                      111 
#       kat cammack             kweisi mfume           lovelynn gwinn 
#               140                        8                        1 
# manga anantatmula mariannette miller meeks         markwayne mullin 
#                24                      100                      127 
#         mo brooks           mondaire jones          pramila jayapal 
#                 2                      242                     1064 
#         ro khanna              steny hoyer          vennia francois 
#              1579                       63                       53 

## Manual adjustment
fb_matched$house <- fb_matched$house %>%
  mutate(
    gender = case_when(
      candidate == "albio sires" ~ "male",
      candidate == "andr carson" ~ "male",
      candidate == "g butterfield" ~ "male",
      candidate == "ilhan omar" ~ "female",
      candidate == "j scholten" ~ "male",
      candidate == "jahana hayes" ~ "female",
      candidate == "kat cammack" ~ "female",
      candidate == "kweisi mfume" ~ "male",
      candidate == "lovelynn gwinn" ~ "female",
      candidate == "manga anantatmula" ~ "female",
      candidate == "mariannette miller meeks" ~ "female",
      candidate == "markwayne mullin" ~ "male",
      candidate == "mo brooks" ~ "male",
      candidate == "mondaire jones" ~ "male",
      candidate == "pramila jayapal" ~ "female",
      candidate == "ro khanna" ~ "male",
      candidate == "steny hoyer" ~ "male",
      candidate == "vennia francois" ~ "female",
      TRUE ~ gender
    )
  )
assert_that(!any(is.na(fb_matched$house$gender)))
assert_that(
  all(fb_matched %>% map_dbl(nrow) == fb_matched_old %>% map_dbl(nrow))
)

# Saving fb_matched ============================================================
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
      )%>%
      mutate(
        ad_creative_body = trimws(gsub("\\s+", " ", ad_creative_body))
      )
  )
assert_that(length(setdiff(tolower(state.abb), names(fb_simple$senate))) == 0)

## Grouped by candidate and ad_creative_body!
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

# Saving fb_meta ===============================================================
save(fb_meta, file = here("data", "tidy", "fb_meta.Rda"))
