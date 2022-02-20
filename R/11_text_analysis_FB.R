source(here::here("R", "utilities.R"))

# Load data ====================================================================
## Text is in "ad_creative_body"
load(here("data", "tidy", "fb_matched.Rda"))
load(here("data", "tidy", "fb_unique.Rda"))
load(here("output", "fb_quanteda.Rda"))

# Adjust candidate labels so that nchar is the same ============================
## Define function, adjust later
temp_fxn <- function(x) {
  x %>%
    map(
      ~ .x %>%
        rowwise() %>%
        mutate(
          candidate = str_pad(
            simple_cap(tolower(candidate)),
            side = "left",
            width = fb_unique %>% map_dbl(~ max(nchar(.x$candidate))) %>% max()
          ),
          party = simple_cap(tolower(party))
        ) %>%
        ungroup() %>%
        ## For now
        filter(party != "Independent")
    )
}

# Top diverse ads or number of ads =============================================
## Note that candidate is the group-level marker, not page_name
## e.g., Luke Letlow For Congress != Luke Letlow, but same candidate
assert_that(
  length(unique(fb_unique$senate$page_name)) != ## 79
    length(unique(fb_unique$senate$candidate)) ## 69
)
assert_that(
  length(unique(fb_unique$house$page_name)) != ## 767
    length(unique(fb_unique$house$candidate)) ## 652
)

## Based on the number of diverse ads or total ads (not accounting for breadth)
fb_unique <- temp_fxn(fb_unique)

# Who among top candidates mention keywords the most? ==========================
top_list <- list()
for (topn in c(10, 30, 1000)) {
  top_unique <- fb_unique %>% map(~ tally_by_cand(.x, lim = topn))
  top_all <- fb_matched %>% map(~ tally_by_cand(.x, lim = topn)) %>% temp_fxn()
  top_all <- temp_fxn(top_all)
  
  temp <- cross2(c("trump", "covid", "chinese"), c("unique", "all")) %>%
    set_names(., nm = {
      .
    } %>% map_chr(~ paste(.x, collapse = "_"))) %>%
    imap(
      ~ {
        var <- paste0("word_", .x[[1]])
        list(
          freq = top_freq(
            l1 = fb_unique, 
            l2 = get(paste0("top_", .x[[2]]), envir = .GlobalEnv),
            var = var
          ) %>%
            map(
              ~ .x %>%
                mutate(
                  ## There must be a smarter way :)
                  candidate = gsub("-c", "-C", candidate),
                  candidate = gsub("Laturner", "LaTurner", candidate),
                  candidate = gsub("Mcsally", "McSally", candidate),
                  candidate = gsub("Mcg", "McG", candidate),
                  candidate = gsub("Mcc", "McC", candidate),
                  candidate = gsub("Mcmorris", "McMorris", candidate),
                  candidate = gsub("Mcadams", "McAdams", candidate),
                  candidate = gsub("Iii", "III", candidate),
                  candidate = gsub("O¬íhalleran", "O'Halleran", candidate),
                  candidate = gsub("-n", "-N", candidate),
                  candidate = gsub("-g", "-G", candidate),
                  candidate = gsub("-s", "-S", candidate),
                  candidate = gsub("-p", "-P", candidate)
                ) %>%
                group_by(candidate) %>%
                mutate(
                  total = sum(n),
                  perc = n / total,
                  perc1 = perc[!!as.name(var) == 1]
                ) %>%
                ungroup()
            ),
          type = .x[[2]],
          var = var,
          lab0 = case_when(
            .x[[1]] == "trump" ~ "Does Not Mention Trump",
            .x[[1]] == "covid" ~ "Does Not Mention COVID-19",
            .x[[1]] == "chinese" ~ "Does Not Mention China"
          ),
          lab1 = case_when(
            .x[[1]] == "trump" ~ "Mentions Trump",
            .x[[1]] == "covid" ~ "Mentions COVID-19",
            .x[[1]] == "chinese" ~ "Mentions China"
          )
        )
      }
    )
  top_list[[paste0("top_", topn)]] <- temp
  
  ## Visualize -----------------------------------------------------------------
  ## Needs labels by state/district
  p1 <- temp %>%
    map(
      ~ list(
        senate = top_freq_plot(
          x = .x, chamber = "senate", top = topn, scales = "free_y",
          height = ifelse(topn == 10, 3.5, 5.2)
        ),
        house = top_freq_plot(
          x = .x, chamber = "house", top = topn, scales = "free_y",
          height = ifelse(topn == 10, 3.5, 5.2)
        )
      )
    )
  
  p2 <- temp %>%
    map(
      ~ list(
        senate = top_freq_plot(
          x = .x, chamber = "senate", top = topn, scales = "free_y",
          height = ifelse(topn == 10, 3.5, 5.2), fxn = "perc"
        ),
        house = top_freq_plot(
          x = .x, chamber = "house", top = topn, scales = "free_y",
          height = ifelse(topn == 10, 3.5, 5.2), fxn = "perc"
        )
      )
    )
}
save(top_list, file = here("output", "word_top_list.Rda"))

## Summary statistics --------------------------------------------------------
top_list %>%
  map_dfr(
    function(x) x %>%
      map_dfr(
        ~ bind_rows(
          .x$freq$senate %>%
            filter(!!as.name(.x$var) == 1) %>%
            group_by(party) %>%
            summarise(perc = mean(perc)) %>%
            mutate(chamber = "Senate"),
          .x$freq$house %>%
            filter(!!as.name(.x$var) == 1) %>%
            group_by(party) %>%
            summarise(perc = mean(perc)) %>%
            mutate(chamber = "House")
        ),
        .id = "case"
      ),
    .id = "topn"
  ) %>%
  filter(grepl("unique", case)) %>%
  filter(grepl("1000", topn))

# # A tibble: 12 x 5
# topn     case           party          perc chamber
# <chr>    <chr>          <chr>         <dbl> <chr>  
#  1 top_1000 trump_unique   Democrat   0.133    Senate 
#  2 top_1000 trump_unique   Republican 0.206    Senate 
#  3 top_1000 trump_unique   Democrat   0.151    House  
#  4 top_1000 trump_unique   Republican 0.158    House  
#  5 top_1000 covid_unique   Democrat   0.0396   Senate 
#  6 top_1000 covid_unique   Republican 0.0465   Senate 
#  7 top_1000 covid_unique   Democrat   0.0569   House  
#  8 top_1000 covid_unique   Republican 0.0415   House  
#  9 top_1000 chinese_unique Democrat   0.00300  Senate 
# 10 top_1000 chinese_unique Republican 0.0463   Senate 
# 11 top_1000 chinese_unique Democrat   0.000895 House  
# 12 top_1000 chinese_unique Republican 0.0177   House 

# Top 3 references by percentage ===============================================
## Limit to n > 10, because otherwise we have n = 1, n = 3 cases
top_list$top_1000$trump_unique$freq %>%
  map_dfr(
    ~ .x %>%
      filter(word_trump == 1) %>%
      group_by(party) %>%
      arrange(desc(perc)) %>% 
      slice(1:3),
    .id = "chamber"
  )

top_list$top_1000$chinese_unique$freq %>%
  map_dfr(
    ~ .x %>%
      filter(word_chinese == 1) %>%
      # Jim Risch, 3 out of 9 (33%), excluded
      filter(total >= 10) %>%
      group_by(party) %>%
      arrange(desc(perc)) %>% 
      slice(1:3),
    .id = "chamber"
  )

fb_unique$senate %>% 
  filter(grepl("Cotton", candidate)) %>%
  .$ad_creative_body
