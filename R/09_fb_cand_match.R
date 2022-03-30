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

## Sanity check
fb_matched$house %>%
  filter(candidate == "cori bush") %>%
  .$inc %>%
  table(useNA = "ifany")

sum(is.na(fb_matched$senate$inc)) ## 147,196 ---> 152
sum(is.na(fb_matched$house$inc)) ## 9,933 ---> 4,737

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

## Senate
fb_matched$senate <- fb_matched$senate %>%
  mutate(
    inc = case_when(
      candidate == "pat toomey" ~ "INCUMBENT",
      candidate == "rand paul" ~ "INCUMBENT",
      TRUE ~ inc
    )
  )

## House
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

# Filling in missing candidates ================================================p
# Tim Kelly
fb_matched[["house"]] <- within(
  fb_matched[["house"]],
  inc[fb_ad_library_id == "107802437414608"] <- "CHALLENGER"
)
# David Torres
fb_matched[["house"]] <- within(
  fb_matched[["house"]],
  inc[fb_ad_library_id == "110300057228517"] <- "CHALLENGER"
)
# Nydia Velazquez
fb_matched[["house"]] <- within(
  fb_matched[["house"]],
  inc[fb_ad_library_id == "110909022379213"] <- "INCUMBENT"
)
fb_matched[["house"]] <- within(
  fb_matched[["house"]],
  candidate[fb_ad_library_id == "110909022379213"] <- "NYDIA VELAZQUEZ"
)
# Kevin Van Ausdal
fb_matched[["house"]] <- within(
  fb_matched[["house"]],
  inc[fb_ad_library_id == "111173810400632"] <- "CHALLENGER"
)
# John Briscoe
fb_matched[["house"]] <- within(
  fb_matched[["house"]],
  inc[fb_ad_library_id == "112634507219876"] <- "CHALLENGER"
)
# Zach Raknerud
fb_matched[["house"]] <- within(
  fb_matched[["house"]],
  inc[fb_ad_library_id == "115454783336289"] <- "CHALLENGER"
)
# TAWNJA ZAHRADKA
fb_matched[["house"]] <- within(
  fb_matched[["house"]],
  inc[fb_ad_library_id == "117188196357573"] <- "CHALLENGER"
)
# QUINN NYSTROM
fb_matched[["house"]] <- within(
  fb_matched[["house"]],
  inc[fb_ad_library_id == "123838185676931"] <- "CHALLENGER"
)
# JESv<U+222B>S G. ¬ìCHUY¬î GARCv<U+2260>A
fb_matched[["house"]] <- within(
  fb_matched[["house"]],
  inc[fb_ad_library_id == "1491458271125644"] <- "INCUMBENT"
)
fb_matched[["house"]] <- within(
  fb_matched[["house"]],
  candidate[fb_ad_library_id == "1491458271125644"] <- "JESUS CHUY GARCIA"
)
# CHRISTY SMITH
fb_matched[["house"]] <- within(
  fb_matched[["house"]],
  inc[fb_ad_library_id == "1517919588502283"] <- "CHALLENGER"
)
# DAVID SCOTT
fb_matched[["house"]] <- within(
  fb_matched[["house"]],
  inc[fb_ad_library_id == "159817223047"] <- "INCUMBENT"
)
# ANGv©LICA MARIA DUEv±AS
fb_matched[["house"]] <- within(
  fb_matched[["house"]],
  inc[fb_ad_library_id == "204623453392865"] <- "CHALLENGER"
)
fb_matched[["house"]] <- within(
  fb_matched[["house"]],
  candidate[fb_ad_library_id == "204623453392865"] <- "ANGELICA MARIA DUENAS"
)
# MICHAEL F. Q. SAN NICOLAS
fb_matched[["house"]] <- within(
  fb_matched[["house"]],
  inc[fb_ad_library_id == "240142306105643"] <- "INCUMBENT"
)
# RANDY K. WEBER, SR.
fb_matched[["house"]] <- within(
  fb_matched[["house"]],
  inc[fb_ad_library_id == "272083892815201"] <- "INCUMBENT"
)
# FRANK D. LUCAS
fb_matched[["house"]] <- within(
  fb_matched[["house"]],
  inc[fb_ad_library_id == "288035911228085"] <- "INCUMBENT"
)
# LINDA T. Sv°NCHEZ
fb_matched[["house"]] <- within(
  fb_matched[["house"]],
  inc[fb_ad_library_id == "29921386086"] <- "INCUMBENT"
)
fb_matched[["house"]] <- within(
  fb_matched[["house"]],
  candidate[fb_ad_library_id == "29921386086"] <- "LINDA SANCHEZ"
)
# TRACY JENNINGS
fb_matched[["house"]] <- within(
  fb_matched[["house"]],
  inc[fb_ad_library_id == "418894668649288"] <- "CHALLENGER"
)
# RICARDO ¬ìRICK¬î DE LA FUENTE
fb_matched[["house"]] <- within(
  fb_matched[["house"]],
  inc[fb_ad_library_id == "508284072665591"] <- "CHALLENGER"
)
fb_matched[["house"]] <- within(
  fb_matched[["house"]],
  candidate[fb_ad_library_id == "508284072665591"] <- "RICARDO DE LA FUENTE"
)
# BEN GIBSON
fb_matched[["house"]] <- within(
  fb_matched[["house"]],
  inc[fb_ad_library_id == "626082571363383"] <- "CHALLENGER"
)
# DAN FEEHAN
fb_matched[["house"]] <- within(
  fb_matched[["house"]],
  inc[fb_ad_library_id == "664770743730817"] <- "CHALLENGER"
)
# RAv<U+222B>L M. GRIJALVA
fb_matched[["house"]] <- within(
  fb_matched[["house"]],
  inc[fb_ad_library_id == "7707979947"] <- "INCUMBENT"
)
fb_matched[["house"]] <- within(
  fb_matched[["house"]],
  candidate[fb_ad_library_id == "7707979947"] <- "RAUL GRIJALVA"
)
# LIZ JOHNSON
fb_matched[["house"]] <- within(
  fb_matched[["house"]],
  inc[fb_ad_library_id == "881875042156266"] <- "CHALLENGER"
)
# GEORGETTE Gv=MEZ
fb_matched[["house"]] <- within(
  fb_matched[["house"]],
  inc[fb_ad_library_id == "927610307291393"] <- "OPEN"
)
fb_matched[["house"]] <- within(
  fb_matched[["house"]],
  candidate[fb_ad_library_id == "927610307291393"] <- "GEORGETTE GOMEZ"
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
