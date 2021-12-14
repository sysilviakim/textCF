# Setup ========================================================================
source(here::here("R", "utilities.R"))
vec <- c(senate = "senate", house = "house")

# Import data ==================================================================
load("data/tidy/cong_complete.Rda")
fb_list <- vec %>%
  map(
    function(x) {
      loadRData(paste0("data/raw/fb/fb-", x, "-raw-ads-2020.Rda")) %>%
        map_dfr("tbl", .id = "candidate")
    }
  )

cand_list <- vec %>%
  map(
    ~ read_csv(
      here("data", "raw", "fb", paste0("fb-", .x, ".csv")),
      col_types = cols(.default = "c")
    ) %>%
      select(where(~ !all(is.na(.x))))
  )

# Match candidate-level characteristics ========================================
## FB original data
fb_matched <- vec %>%
  map(
    ~ cong_complete[[.x]] %>% 
      clean_names() %>%
      select(
        -year, -contains("stage"), -contains("special"),
        -contains("candidatevotes"), -contains("totalvotes"),
        -contains("party")
      ) %>%
      left_join(
        left_join(fb_list[[.x]], cand_list[[.x]]) %>% 
          rename(state_name = state) %>%
          mutate(candidate = trimws(gsub('"', '', candidate))),
        .
      )
  )
  
save(fb_matched, file = here("data", "tidy", "fb_matched.Rda"))
