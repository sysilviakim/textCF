source(here::here("R", "utilities.R"))
library(fixest)

# Load data ====================================================================
load(here("output", "persp_final_results.Rda"))
load(here("data", "tidy", "fb_unique.Rda"))

# Bind and merge ===============================================================
fb <- fb_unique %>%
  bind_rows(., .id = "chamber") %>%
  mutate(chamber = case_when(chamber == "senate" ~ "Senate", TRUE ~ "House"))

## Having to do this means that the code needs to be fixed somewhere
## Incumbency status also missing from somewhere;
## Currently patched up. Must fix
merged <- left_join(
  df %>%
    clean_names() %>%
    select(-contains("word"), -contains("state")) %>%
    select(-donate, -financial, -vote_share, -type, -inc, -party, -chamber) %>%
    dedup(),
  fb %>%
    select(-n) %>%
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
      as.Date(round(max_ad_delivery_stop_time), origin = "1970-01-01")
  )

temp <- merged %>%
  filter(!is.na(party) & !is.na(state_po) & party != "INDEPENDENT") %>%
  mutate(inc = case_when(is.na(inc) ~ "CHALLENGER", TRUE ~ inc)) %>%
  party_factor(., outvar = "Type")

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

# OLS first for reference ======================================================

fit <- lm(
  toxicity ~
  party * financial + chamber + inc + min_ad_delivery_start_time + state_po,
  temp
)
summary(fit)

fit_se_cluster <- feols(
  toxicity ~
  party * financial + chamber + inc + min_ad_delivery_start_time + state_po,
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
