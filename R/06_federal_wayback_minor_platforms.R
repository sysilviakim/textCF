source(here::here("R", "utilities.R"))

# Load merged data + main platforms ============================================
load(here("data", "tidy", "cong_merged_donations.Rda"))
load(here("data", "tidy", "actblue_congress.Rda"))
load(here("data", "tidy", "winred_congress.Rda"))
load(here("data", "tidy", "anedot_congress.Rda"))

# What's missing? ==============================================================
cong_raw <- cong
cong <- cong_raw %>%
  imap(
    ~ anti_join(
      wayback_std(.x),
      Reduce(bind_rows, list(actblue[[.y]], winred[[.y]], anedot[[.y]])),
      by = "url"
    ) %>%
      ## Can't go back and see PayPal
      filter(!grepl("paypal", url))
)

## 14 and 130
cong %>% map_dbl(nrow)

# NGP VAN ======================================================================
minor_platforms(cong, "ngp")
ngp <- save_text_df("ngp")
ngp %>% map_dbl(nrow)
save(ngp, file = here("data", "tidy", "ngp_congress.Rda"))

# Donorbox =====================================================================
minor_platforms(cong, "donorbox")
donorbox <- save_text_df("donorbox")
donorbox %>% map_dbl(nrow)
save(donorbox, file = here("data", "tidy", "donorbox_congress.Rda"))

# Duplicate scrapes per URL ====================================================

