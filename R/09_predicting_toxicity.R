source(here::here("R", "utilities.R"))
library(fixest)

# Load data ====================================================================
load(here("output", "persp_final_results.Rda"))
load(here("data", "tidy", "fb_unique.Rda"))

# Bind and merge ===============================================================
fb <- fb_unique %>% bind_rows(., .id = "chamber")

## Having to do this means that the code needs to be fixed somewhere
## Incumbency status also missing from somewhere;
## Currently patched up. Must fix
merged <- left_join(
  ## Joining, by = c("candidate", "fb_ad_library_id", "page_name",
  ##                 "ad_creative_body", "ad_creative_link_caption")
  ## Everything in this subsetted df should be matched
  df %>%
    clean_names() %>%
    dedup() %>%
    clean_candidate() %>%
    filter(candidate != "michael san nicolas"),
  fb %>%
    select(-n) %>%
    filter(party %in% c("DEMOCRAT", "REPUBLICAN") & !is.na(party)) %>%
    dedup()
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
      party == "DEMOCRAT" ~ pvi,
      party == "REPUBLICAN" ~ -pvi
    )
  )

## Only non-running Senators
assert_that(
  merged %>%
    filter(
      is.na(state_po) & chamber != "senate" & candidate != "tracy jennings"
    ) %>%
    nrow() == 0
)
assert_that(
  merged %>%
    filter(
      is.na(party) & chamber != "senate" & candidate != "tracy jennings"
    ) %>%
    nrow() == 0
)

temp <- merged %>%
  filter(!is.na(party) & !is.na(state_po) & party != "INDEPENDENT") %>%
  party_factor(., outvar = "Type") %>%
  mutate(
    financial = factor(
      financial,
      levels = c("Voter-targeting", "Donor-targeting")
    )
  ) %>%
  select(financial, everything())

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

# Very similar ads? ============================================================
simil_df <- temp %>%
  group_by(candidate, toxicity) %>%
  slice(1)

nrow(simil_df) / nrow(temp) * 100 ## 94.2% of ads preserved

fit_se_cluster2 <- feols(
  toxicity ~
    party * financial + chamber + inc + safety + gender +
    min_ad_delivery_start_time + state_po,
  simil_df
)

summary(fit_se_cluster2, cluster = ~candidate)
etable(fit_se_cluster2, cluster = "candidate")
etable(
  fit_se_cluster2,
  cluster = "candidate", tex = TRUE,
  file = here("tab", "fit_cand_cluster_toxicity2.tex")
)

fit_fe2 <- feols(
  toxicity ~
    financial + min_ad_delivery_start_time | candidate,
  simil_df
)

summary(fit_fe2)
etable(fit_fe2)
etable(fit_fe2, tex = TRUE, file = here("tab", "fit_fe_toxicity2.tex"))

# Leave-one-candidate-out estimation ===========================================
cand_list <- unique(temp$candidate)
fe_loo_list <- cand_list %>%
  set_names(., .) %>%
  map(
    ~ feols(
      toxicity ~
        financial + min_ad_delivery_start_time | candidate,
      temp %>% filter(candidate != .x)
    )
  )

fe_loo_summ <- fe_loo_list %>%
  map_dfr(~ confint(summary(.x))[1, ], .id = "Candidate") %>%
  select(Candidate, everything()) %>%
  `rownames<-`(NULL)

fe_loo_summ %>%
  filter(`2.5 %` < 0 & `97.5 %` > 0)
#            Candidate         2.5 %    97.5 %
# 1    raphael warnock -0.0000098437 0.0078351
# 2     jaime harrison -0.0001326602 0.0076486
# 3        rishi kumar -0.0001396589 0.0075349
# 4 ammar campa najjar -0.0000577856 0.0077266
assert_that(fe_loo_summ %>% filter(`2.5 %` < 0 & `97.5 %` < 0) %>% nrow() == 0)
save(
  list = c("fe_loo_list", "fe_loo_summ"),
  file = here("output", "fe_loo.Rda")
)

by_cand_type <- temp %>%
  group_by(candidate, financial) %>%
  summarise(toxicity = mean(toxicity, na.rm = TRUE)) %>%
  pivot_wider(
    id_cols = "candidate", names_from = "financial", values_from = "toxicity"
  ) %>%
  clean_names() %>%
  filter(!is.na(voter_targeting) & !is.na(donor_targeting)) %>%
  mutate(diff = donor_targeting - voter_targeting) %>%
  arrange(desc(diff)) %>%
  left_join(., temp %>% group_by(candidate) %>% summarise(n = n()))


# Estimating text similarity ===================================================
load(here("output", "fb_quanteda.Rda"))
## simil_output <- textstat_simil(dfm_FB_ad)
##   ---> gives allocate vector error (18.2 Gb)

if (!file.exists(here("output", "simil_corr_list.Rda"))) {
  simil_cosine_list <- simil_corr_list <- 
    vector("list", length = nrow(dfm_FB_ad))
  for (i in seq(nrow(dfm_FB_ad))) {
    ## Default = correlation
    ## Jaro-Winkler string distance?
    simil_corr_list[[i]] <- textstat_simil(dfm_FB_ad[i, ], dfm_FB_ad)
    simil_cosine_list[[i]] <-
      textstat_simil(dfm_FB_ad[i, ], dfm_FB_ad, method = "cosine")
    message(paste0(i, "-th similarity computed."))
    
    if ((i %% 100 == 0) | nrow(dfm_FB_ad) == i) {
      save(simil_corr_list, file = here("output", "simil_corr_list.Rda"))
      save(simil_cosine_list, file = here("output", "simil_cosine_list.Rda"))
    }
  }
}
