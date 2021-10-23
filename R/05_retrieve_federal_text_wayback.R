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
      mutate(
        file = wayback_stamp_html(
          ., row_number(), 
          here("data", "raw", "wayback", .x, "timestamp_donation")
        )
      ) %>%
      filter(file.exists(file)) %>%
      mutate(gap = abs(date - as.Date("2020-11-03"))) %>%
      group_by(url) %>%
      ## Choose date closest to Election Day, before Election Day
      ## If after Election Day, allow 10 days
      ## Must be a file that is actually downloaded
      filter(gap == min(gap)) %>%
      filter(date == min(date)) %>%
      slice(1) %>%
      ## Delete what was initially merged
      select(-title, -text)
  )

## Must be senate 142 and house 822, but 138 and 675
cong %>% map_dbl(nrow)

# Check missing files ==========================================================
raw <- list(
  senate = loadRData(here("data", "tidy", "senate-2020-matched.Rda")) %>%
    mutate(state_cd = paste0(state, "-0")),
  house = loadRData(here("data", "tidy", "house-2020-matched.Rda")) %>%
    ## manual fix
    mutate(
      url = case_when(
        last_name == "swisher" & grepl("mostpeople", url) ~ 
          "https://secure.ngpvan.com/v1/Forms/WuiklsKYhk223Z9Pkldcyg2",
        TRUE ~ url
      )
    )
) %>%
  map(~ .x %>% group_by(url) %>% slice(1))

View(anti_join(wayback_std(raw$senate), wayback_std(cong$senate), by = "url"))
View(anti_join(wayback_std(raw$house), wayback_std(cong$house), by = "url"))

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
cong %>%
  imap(
    ~ {
      df <- .x %>% filter(grepl("winred", url))
      winred_text_list <- vector(mode = "list", length = nrow(df))
      
      fp <- here("data", "raw", "wayback", .y, "html")
      
      for (x in seq(nrow(df))) {
        full_js <- out <- NA
        
        tryCatch(
          {
            winred_text_list[[x]] <- 
              winred_text_scrape(wayback_stamp_html(df, x, fp)) %>%
              select(-date)
            winred_text_list[[x]] <-
              bind_cols(winred_text_list[[x]], df[x, ]) %>%
              mutate(file = url) %>%
              mutate(url = df$url[x])
          },
          error = function(e) {
            message(e)
          }
        )

        if ((x %% 50 == 0) | x == nrow(df)) {
          tryCatch({
            save(
              winred_text_list, 
              file = here(
                "data", "raw", 
                paste0("winred_text_", .y, "_list.Rda")
              )
            )
          }, error = function(e) {
            message(e)
          })
        }
      }
    }
  )

winred <- c(senate = "senate", house = "house") %>%
  map(
    ~ loadRData(
      here("data", "raw",  paste0("winred_text_", .x, "_list.Rda"))
    ) %>%
      keep(~ !is.null(.x)) %>%
      map(clean_names) %>%
      bind_rows() %>%
      clean_names()
  )

# Anedot text ==================================================================

# Misc. text ===================================================================


