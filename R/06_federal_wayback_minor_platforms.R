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
      Reduce(bind_rows, list(actblue[[.y]], winred[[.y]], anedot[[.y]])) %>%
        ## Can't go back and see PayPal
        filter(!grepl("paypal", url)),
      by = "url"
    )
)

## 16 and 137
cong %>% map_dbl(nrow)

# NGP VAN ======================================================================
cong %>%
  imap(
    ~ {
      ## Three types: act.myngp, actions.ngpvan, secure.ngpvan
      df <- .x %>% filter(grepl("ngp", url))
      out <- vector(mode = "list", length = nrow(df))
      fp <- here("data", "raw", "wayback", .y, "html")
      
      for (x in seq(nrow(df))) {
        tryCatch(
          {
            ## Because this relies on RSelenium, need to access
            ## WayBack machine directly
            out[[x]] <- ngp_select_text(df$link[x]) %>%
              select(-date)
            out[[x]] <- bind_cols(
              out[[x]] %>% rename(link = url), df[x, c("url")]
            ) %>%
              select(url, -link, everything())
          },
          error = function(e) {
            message(e)
          }
        )
        
        if ((x %% 50 == 0) | x == nrow(df)) {
          save(
            out,
            file = here("data/raw", paste0("ngpvan_text_", .y, "_list.Rda"))
          )
        }
      }
    }
  )


# Duplicate scrapes per URL ====================================================

