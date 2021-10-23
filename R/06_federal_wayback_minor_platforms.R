source(here::here("R", "utilities.R"))

# Load merged data + main platforms ============================================
load(here("data", "tidy", "cong_merged_donations.Rda"))
load(here("data", "tidy", "actblue_congress.Rda"))
load(here("data", "tidy", "winred_congress.Rda"))
load(here("data", "tidy", "anedot_congress.Rda"))

# What's missing? ==============================================================
x <- "senate"
test <- Reduce(bind_rows, list(actblue[[x]], winred[[x]], anedot[[x]]))
##  14 rows ( 9.7%)
View(
  anti_join(wayback_std(cong[[x]]), test, by = "url") %>%
    ## Can't go back and see PayPal
    filter(!grepl("paypal", url))
)

## 130 rows (15.9%)
x <- "house"
test <- Reduce(bind_rows, list(actblue[[x]], winred[[x]], anedot[[x]]))
View(
  anti_join(wayback_std(cong[[x]]), test, by = "url") %>%
    filter(!grepl("paypal", url))
)

# Duplicate scrapes per URL ====================================================

