source(here::here("R", "utilities.R"))
library(fixest)

# Load data ====================================================================
load(here("output", "persp_final_results.Rda"))
load(here("data", "tidy", "fb_unique.Rda"))

# Bind and merge ===============================================================
fb <- fb_unique %>% bind_rows(., .id = "chamber") %>%
  mutate(chamber = case_when(chamber == "senate" ~ "Senate", TRUE ~ "House"))

## Having to do this means that the code needs to be fixed somewhere
## Incumbency status also missing from somewhere;
## Currently patched up. Must fix
merged <- left_join(
  ## Joining, by = c("candidate", "fb_ad_library_id", "page_name", "ad_creative_body", "ad_creative_link_caption")
  ## Everything in this subsetted df should be matched
  df %>%
    clean_names() %>%
    select(-contains("word"), -contains("state")) %>%
    select(-donate, -financial, -vote_share, -type, -inc, -chamber) %>%
    dedup() %>%
    clean_candidate() %>%
    filter(party %in% c("Democrat", "Republican")) %>%
    filter(candidate != "michael san nicolas"),
  fb %>% select(-n, -party) %>% dedup()
) %>%
  dedup() %>%
  mutate(
    mean_ad_delivery_start_time =
      as.Date(round(mean_ad_delivery_start_time), origin = "1970-01-01"),
    mean_ad_delivery_stop_time =
      as.Date(round(mean_ad_delivery_stop_time), origin = "1970-01-01"),
    min_ad_delivery_start_time =
      as.Date(round(min_ad_delivery_start_time), origin = "1970-01-01"),
    min_ad_delivery_stop_time =
      as.Date(round(min_ad_delivery_stop_time), origin = "1970-01-01"),
    max_ad_delivery_start_time =
      as.Date(round(max_ad_delivery_start_time), origin = "1970-01-01"),
    max_ad_delivery_stop_time =
      as.Date(round(max_ad_delivery_stop_time), origin = "1970-01-01"),
    safety = case_when(
      party == "Democrat" ~ pvi,
      party == "Republican" ~ -pvi
    )
  )

## Only non-running Senators
assert_that(
  merged %>% filter(is.na(state_po) & chamber != "Senate") %>% nrow() == 0
)
assert_that(
  merged %>% filter(is.na(party) & chamber != "Senate") %>% nrow() == 0
)

temp <- merged %>%
  filter(!is.na(party) & !is.na(state_po) & party != "INDEPENDENT") %>%
  party_factor(., outvar = "Type") %>%
  mutate(
    financial = factor(
      financial,
      levels = c("Voter-targeting", "Donor-targeting")
    )
  )

## Assertions for sanity check
assert_that(!any(is.na(temp$inc)))
assert_that(!any(is.na(temp$pvi)))
assert_that(!any(is.na(temp$chamber)))
assert_that(!any(is.na(temp$state_po)))
assert_that(!any(is.na(temp$party)))
nrow(temp)

# Over time plot ===============================================================
color4_modified <- color4
names(color4_modified) <- gsub("\n", " ", names(color4_modified))
p <- temp %>%
  mutate(year_month = floor_date(min_ad_delivery_start_time, "month")) %>%
  filter(year_month >= as.Date("2019-01-01")) %>%
  mutate(Type = gsub("\n", " ", Type)) %>%
  group_by(Type, year_month) %>%
  count() %>%
  ggplot(aes(x = year_month, y = n, group = Type, color = Type)) +
  geom_line() +
  scale_color_manual(values = color4_modified) +
  xlab("Election Cycle, Monthly") +
  ylab("Number of Unique Ads") +
  scale_x_date(breaks = "3 months", date_labels = "%Y\n%b") +
  scale_y_continuous(labels = scales::comma)

pdf(here("fig", "n_unique_ads_over_time.pdf"), width = 4.0, height = 3.2)
plot_notitle(pdf_default(p)) + theme(legend.position = "bottom") +
  guides(colour = guide_legend(nrow = 2))
dev.off()

# OLS first for reference (simple model) =======================================
fit <- lm(
  toxicity ~
  party * financial + chamber + inc + safety + gender + 
    min_ad_delivery_start_time + state_po,
  temp
)
summary(fit)

fit_se_cluster <- feols(
  toxicity ~
  party * financial + chamber + inc + safety + gender + 
    min_ad_delivery_start_time + state_po,
  temp
)

summary(fit_se_cluster, cluster = ~candidate)
etable(fit_se_cluster, cluster = "candidate")
etable(
  fit_se_cluster,
  cluster = "candidate", tex = TRUE,
  file = here("tab", "fit_cand_cluster_toxicity.tex")
)

fit_fe <- feols(
  toxicity ~
  financial + min_ad_delivery_start_time | candidate,
  temp
)

summary(fit_fe)
etable(fit_fe)
etable(fit_fe, tex = TRUE, file = here("tab", "fit_fe_toxicity.tex"))
