source(here::here("R", "utilities.R"))
options(digits = 3, scipen = 999)

# Load and wrangle data ========================================================
fb_unique <- read_rds(here("data", "tidy", "toxicity.RDS")) %>%
  ## Using max_ad_delivery_start_time, divide the 2020 election cycle
  ## into pre-primary, primary, and general election period
  mutate(
    election_cycle = case_when(
      ## Iowa caucuses were on Feb 3, but early voting began in Minnesota
      ## on Jan 17. Considering this, just use Jan 1, 2020 as the end of
      ## the pre-primary period
      max_ad_delivery_start_time < as.Date("2020-01-01") ~ "Pre-primary",
      max_ad_delivery_start_time >= as.Date("2020-01-01") &
        max_ad_delivery_start_time < as.Date("2020-04-09") ~ "Primary",
      ## Sanders dropped out on April 8, 2020 and endorsed Biden
      ## as the presumptive nominee
      max_ad_delivery_start_time >= as.Date("2020-04-09") ~ "General"
    ),
    election_cycle = factor(
      election_cycle,
      levels = c("Pre-primary", "Primary", "General"),
    )
  )

# Summary statistics: given general candidates, % of ads in each period? =======
prop(fb_unique, "election_cycle")
# Pre-primary     Primary     General
#        28.2        14.1        57.7

fb_unique %>%
  group_by(party, election_cycle) %>%
  summarise(n = n()) %>%
  mutate(prop = n / sum(n))
#   party      election_cycle     n  prop
#   <chr>      <fct>          <int> <dbl>
# 1 Democrat   Pre-primary    10856 0.326
# 2 Democrat   Primary         4549 0.137
# 3 Democrat   General        17894 0.537
# 4 Republican Pre-primary     5079 0.218
# 5 Republican Primary         3437 0.148
# 6 Republican General        14770 0.634

# Group by election cycle, split, and create summary figures ===================
cycle_list <- fb_unique %>%
  group_by(election_cycle) %>%
  group_split() %>%
  `names<-`({
    .
  } %>% map(~ .x$election_cycle[1]) %>% unlist()) %>%
  map(
    ## Within the groups, do what we did in perspective FB results script
    ~ summ_df_fxn(.x %>% group_by(party, financial, chamber))
  )

fb_perspective_plot(
  cycle_list$`Pre-primary`,
  xvar = "Toxic", se = "se_toxic",
  xlab = paste0(
    "Average Probability that Ad Text Would Be\n",
    "Perceived as Rude/Disrespectful/Toxic"
  )
)
ggsave(here("fig", "pre_primary_toxicity.pdf"), width = 6, height = 2.8)

fb_perspective_plot(
  cycle_list$Primary,
  xvar = "Toxic", se = "se_toxic",
  xlab = paste0(
    "Average Probability that Ad Text Would Be\n",
    "Perceived as Rude/Disrespectful/Toxic"
  )
)
ggsave(here("fig", "primary_toxicity.pdf"), width = 6, height = 2.8)

fb_perspective_plot(
  cycle_list$General,
  xvar = "Toxic", se = "se_toxic",
  xlab = paste0(
    "Average Probability that Ad Text Would Be\n",
    "Perceived as Rude/Disrespectful/Toxic"
  )
)
ggsave(here("fig", "general_toxicity.pdf"), width = 6, height = 2.8)
