source(here::here("R", "utilities.R"))

# Load data ====================================================================
## Text is in "ad_creative_body"
load(here("data", "tidy", "fb_matched.Rda"))
load(here("data", "tidy", "fb_unique.Rda"))
load(here("output", "fb_quanteda.Rda"))

# Adjust candidate labels so that nchar is the same ============================
fb_unique <- fb_unique %>%
  map(
    ~ .x %>%
      rowwise() %>%
      mutate(
        candidate = str_pad(
          simple_cap(tolower(candidate)), side = "left",
          width = fb_unique %>% map_dbl(~ max(nchar(.x$candidate))) %>% max()
        ),
        party = simple_cap(tolower(party))
      ) %>%
      ungroup()
  )

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
top_unique <- fb_unique %>%
  map(tally_by_cand) %>%
  map(~ .x %>% filter(party != "INDEPENDENT"))
top_all <- fb_matched %>%
  map(tally_by_cand) %>%
  map(~ .x %>% filter(party != "INDEPENDENT"))

# Who among top candidates mention keywords the most? ==========================
top_list <- cross2(c("trump", "covid", "chinese"), c("unique", "all")) %>%
  set_names(., nm = {
    .
  } %>% map_chr(~ paste(.x, collapse = "_"))) %>%
  imap(
    ~ list(
      freq = top_freq(
        l1 = fb_unique, l2 = get(paste0("top_", .x[[2]]), envir = .GlobalEnv),
        var = paste0("word_", .x[[1]])
      ),
      var = paste0("word_", .x[[1]]),
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
  )

## Needs labels by state/district + better colors (not blue) +
## unification of y-axis label size by paste + nchar
p_list <- top_list %>%
  map(
    ~ list(
      senate = top_freq_plot(x = .x, chamber = "senate", scales = "free_y"),
      house = top_freq_plot(x = .x, chamber = "house", scales = "free_y")
    )
  )
