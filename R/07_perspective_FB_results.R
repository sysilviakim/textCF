source(here::here("R", "utilities.R"))

# Load data ====================================================================
load(here("data", "tidy", "fb_matched.Rda"))
load(here("data", "tidy", "fb_unique.Rda"))

## Wrangle data
fb_matched <- fb_matched %>%
  map(
    ~ .x %>%
      select(
        candidate, fb_ad_library_id, page_name, party, inc, state_po, pvi, 
        gender, proportion_female, contains("state_cd"), 
        ad_creative_body, ad_creative_link_caption, vote_share
      ) %>%
      mutate(ad_creative_body = trimws(gsub("\\s+", " ", ad_creative_body))) %>%
      filter(ad_creative_body != "") %>%
      filter(ad_creative_body != "{{product.brand}}") %>%
      filter(!(page_name == "Antonio Delgado" & party == "REPUBLICAN")) %>%
      mutate(ad_creative_link_caption = tolower(ad_creative_link_caption)) %>%
      rowwise() %>%
      simplify_ad_body() %>%
      simplify_ad_body() %>%
      mutate(
        type = case_when(
          grepl("actblue.com", ad_creative_link_caption) ~ "ActBlue",
          grepl("winred.com", ad_creative_link_caption) ~ "WinRed",
          grepl("ngpvan.com|myngp.com", ad_creative_link_caption) ~ "NGP VAN",
          grepl("anedot.com", ad_creative_link_caption) ~ "Anedot",
          grepl("victorypassport.com", ad_creative_link_caption) ~
            "Victory Passport",
          grepl("fundraiser", ad_creative_link_caption) ~ "Misc.",
          is.na(ad_creative_link_caption) ~ "Voter-targeting",
          grepl(
            "conversation with |town hall|meet |tour stop |iwillvote.com",
            ad_creative_link_caption
          ) ~ "Voter-targeting",
          grepl(".gov", ad_creative_link_caption) ~ "Government Information",
          grepl("secure.|act.|action.|go.", ad_creative_link_caption) ~
            "Personal Contribution Link",
          grepl("facebook.com|fb.me", ad_creative_link_caption) ~
            "Facebook Page",
        )
      ) %>%
      donate_classify() %>%
      ungroup()
  )
save(fb_matched, file = here("data", "tidy", "fb_matched_wrangled.Rda"))

## Load the latest perspective results
load(
  max(
    list.files(
      here("output"),
      pattern = "persp_final_results_", full.names = TRUE
    )
  )
)

df <- df %>% select(candidate, ad_creative_body, chamber, toxicity, obscene)

# Merge data ===================================================================
## This is the main dataset that will be used
df_unique <- bind_rows(
  left_join(fb_unique$senate, df %>% filter(chamber == "senate")) %>%
    mutate(chamber = "Senate"),
  left_join(fb_unique$house, df %>% filter(chamber == "house")) %>%
    mutate(chamber = "House")
) %>%
  rowwise() %>%
  mutate(
    party = simple_cap(tolower(party)),
    financial = simple_cap(tolower(financial))
  ) %>%
  ungroup()
assert_that(!any(is.na(df_unique$toxicity)))
save(df_unique, file = here("data", "tidy", "merged_unique.Rda"))

## Just in case we want to perform weighting with duplicate-included
## dataset, keep all data
df_all <- bind_rows(
  left_join(fb_matched$senate, df %>% filter(chamber == "senate")) %>%
    mutate(chamber = "Senate"),
  left_join(fb_matched$house, df %>% filter(chamber == "house")) %>%
    mutate(chamber = "House")
) %>%
  rowwise() %>%
  mutate(
    party = simple_cap(tolower(party)),
    financial = simple_cap(tolower(financial))
  ) %>%
  ungroup()
assert_that(!any(is.na(df_all$toxicity)))
save(df_all, file = here("data", "tidy", "merged_all.Rda"))

# Wrangle summary dataset ======================================================
summ_unique <- summ_df_fxn(df_unique %>% group_by(party, financial, chamber))
summ_all <- summ_df_fxn(df_all %>% group_by(party, financial, chamber))

summ_donor_unique <- summ_df_fxn(
  df_unique %>%
    filter(financial == "Donor-targeting") %>%
    mutate(
      type = case_when(
        type %in% c("WinRed", "ActBlue") ~ type,
        type != "WinRed" & party == "Republican" ~ "Other Rep.\nPlatform",
        type != "ActBlue" & party == "Democrat" ~ "Other Dem.\nPlatform"
      )
    ) %>%
    filter(!is.na(type)) %>%
    group_by(chamber, type),
  full = TRUE
)

summ_donor_all <- summ_df_fxn(
  df_all %>%
    filter(financial == "Donor-targeting") %>%
    mutate(
      type = case_when(
        type %in% c("WinRed", "ActBlue") ~ type,
        type != "WinRed" & party == "Republican" ~ "Other Rep.\nPlatform",
        type != "ActBlue" & party == "Democrat" ~ "Other Dem.\nPlatform"
      )
    ) %>%
    filter(!is.na(type)) %>%
    group_by(chamber, type),
  full = TRUE
)

# T-tests ======================================================================
## An example for when we'd need them in the main text
df_unique %>%
  filter(chamber == "House" & party == "Democrat") %>%
  t.test(toxicity ~ financial, data = .)

# Level of Toxicity by Facebook Ad Type ========================================
p1 <- fb_perspective_plot(
  summ_unique,
  xvar = "Toxic", se = "se_toxic",
  xlab = paste0(
    "Average Probability that Ad Text Would Be\n",
    "Perceived as Rude/Disrespectful/Toxic"
  )
)

pdf(
  here("fig", "fb_toxic_means_by_type_chamber.pdf"),
  width = 6, height = 2.8
)
print(p1)
dev.off()

p2 <- fb_perspective_plot(
  summ_all,
  xvar = "Toxic", se = "se_toxic",
  xlab = paste0(
    "Average Probability that Ad Text Would Be\n",
    "Perceived as Rude/Disrespectful/Toxic"
  )
)

pdf(
  here("fig", "fb_toxic_means_by_type_chamber_all.pdf"),
  width = 6, height = 2.8
)
print(p2)
dev.off()
