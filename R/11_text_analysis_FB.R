source(here::here("R", "utilities.R"))

# Load data ====================================================================
## Text is in "ad_creative_body"
load(here("data", "tidy", "fb_matched.Rda"))
load(here("data", "tidy", "fb_unique.Rda"))
load(here("output", "fb_quanteda.Rda"))

# Top 30 diverse ads or number of ads ==========================================

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
top_unique <- fb_unique %>% map(tally_by_cand)
top_all <- fb_matched %>% map(tally_by_cand)

# Who among top candidates mention keywords the most? ==========================
top_list <- list(
  trump_unique = top_freq(fb_unique, top_unique, var = "word_trump"),
  trump_all = top_freq(fb_unique, top_all, var = "word_trump"),
  covid_unique = top_freq(fb_unique, top_unique, var = "word_covid"),
  covid_all = top_freq(fb_unique, top_all, var = "word_covid"),
  chinese_unique = top_freq(fb_unique, top_unique, var = "word_chinese"),
  chinese_all = top_freq(fb_unique, top_all, var = "word_chinese")
)

## Needs better distinction by party + state/district + better colors (not blue)
top_freq_plot(
  df = top_list$trump_unique$senate,
  var = "word_trump", lab0 = "Does Not Mention Trump", lab1 = "Mentions Trump"
)

top_freq_plot(
  df = top_list$trump_all$house,
  var = "word_trump", lab0 = "Does Not Mention Trump", lab1 = "Mentions Trump"
)