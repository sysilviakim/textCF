source(here::here("R", "utilities.R"))

# Read merged data =============================================================
## For consistency, use *all* available WayBack snapshots for *all* candidates
cong <- c(senate = "senate", house = "house") %>%
  imap(
    ~ loadRData(
      here(
        "data", "tidy", "wayback", .x,
        paste0("wayback_", .x, "_merged_donation.Rda")
      )
    ) %>%
      mutate(gap = abs(date - as.Date("2020-11-03"))) %>%
      group_by(url) %>%
      filter(gap < 11) %>%
      ## Choose date closest to Election Day, before Election Day
      ## If after Election Day, allow 10 days
      filter(gap == min(gap)) %>%
      filter(date == min(date)) %>%
      slice(1)
  )

## senate 136 and house 692
cong %>% map_dbl(nrow)

# ActBlue text =================================================================
cong %>%
  imap(
    ~ {
      df <- .x %>% filter(grepl("actblue", url))
      actblue_js_list <- vector(mode = "list", length = nrow(df))
      
      fp <- here("data", "raw", "wayback", .y, "html")
      
      for (x in seq(nrow(df))) {
        full_js <- out <- NA
        
        tryCatch({
          full_js <- actblue_js(wayback_stamp_html(df, x, fp)) %>%
            mutate(file = url) %>%
            mutate(url = df$url[x])
        }, error = function(e) {
          message(e)
        })
        
        if (any(class(full_js) == "tbl")) {
          actblue_js_list[[x]] <- full_js
          message(paste0(x, "-th complete: campaign for ", df$url[x]))
        } else {
          message(paste0("Javascript did not scrape for ", df$url[x]))
        }
        
        if ((x %% 50 == 0) | x == nrow(df)) {
          tryCatch({
            save(
              actblue_js_list, 
              file = here(
                "data", "raw", 
                paste0("actblue_js_", .y, "_list.Rda")
              )
            )
          }, error = function(e) {
            message(e)
          })
        }
      }
    }
  )

actblue <- c(senate = "senate", house = "house") %>%
  map(
    ~ loadRData(
      here("data", "raw",  paste0("actblue_js_", .x, "_list.Rda"))
    ) %>%
      bind_rows() %>%
      actblue_wrangle()
  )

# WinRed text ==================================================================


# Anedot text ==================================================================

# Misc. text ===================================================================


