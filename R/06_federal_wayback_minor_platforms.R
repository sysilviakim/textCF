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

## 14 and 127
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

# Efundraising =================================================================
minor_platforms(cong, "efundraising")
efundraising <- save_text_df("efundraising")
efundraising %>% map_dbl(nrow)
save(efundraising, file = here("data", "tidy", "efundraising_congress.Rda"))

# Fundhero =====================================================================
minor_platforms(cong, "fundhero")
fundhero <- save_text_df("fundhero")
fundhero %>% map_dbl(nrow)
save(fundhero, file = here("data", "tidy", "fundhero_congress.Rda"))

# Raise the money ==============================================================
minor_platforms(cong, "raisethemoney")
raisethemoney <- save_text_df("raisethemoney")
raisethemoney %>% map_dbl(nrow)
save(raisethemoney, file = here("data", "tidy", "raisethemoney_congress.Rda"))

# Click and pledge =============================================================
minor_platforms(cong, "clickandpledge")
clickandpledge <- save_text_df("clickandpledge")
clickandpledge %>% map_dbl(nrow)
save(clickandpledge, file = here("data", "tidy", "clickandpledge_congress.Rda"))

# Authorize.net ================================================================
minor_platforms(cong, "authorize")
authorize <- save_text_df("authorize")
authorize %>% map_dbl(nrow)
save(authorize, file = here("data", "tidy", "authorize_congress.Rda"))

# Transaxt =====================================================================
minor_platforms(cong, "transaxt")
transaxt <- save_text_df("transaxt")
transaxt %>% map_dbl(nrow)
save(transaxt, file = here("data", "tidy", "transaxt_congress.Rda"))

# Piryx ========================================================================
minor_platforms(cong, "piryx")
piryx <- save_text_df("piryx")
piryx %>% map_dbl(nrow)
save(piryx, file = here("data", "tidy", "piryx_congress.Rda"))

# Numero.ai ====================================================================
minor_platforms(cong, "numero")
numero <- save_text_df("numero")
numero %>% map_dbl(nrow)
save(numero, file = here("data", "tidy", "numero_congress.Rda"))
