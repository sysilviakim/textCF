library(peRspective)
source(here::here("R", "utilities.R"))
load(here("data", "tidy", "fb_unique.Rda"))
vec <- c(senate = "senate", house = "house")

# Estimate House/Senate data with perspective API ==============================
for (chamber in vec) {
  df <- fb_unique[[chamber]] %>% mutate(persp_id = row_number())
  
  ## Batch-loop
  batch_list <- vector("list", (nrow(df) %/% 1e2) + 1)
  for (i in seq(nrow(df) %/% 1e2 + 1)) {
    batch_list[[i]] <- df %>%
      filter(persp_id %in% seq(1e2 * (i - 1) + 1, max(1e2 * i, nrow(df)))) %>%
      prsp_stream(
        text = ad_creative_body,
        text_id = persp_id,
        languages = "en",
        score_model = c("TOXICITY", "OBSCENE")
      )
    print(paste0(i, "-th batch finished for ", chamber, "."))
    save(batch_list, file = here("out", paste0("batch_", chamber, "_list.Rda")))
  }
  print("All batches finished!! ==============================================")
  
  fb_merged <- left_join(
    df, 
    batch_list %>% bind_rows() %>% rename(persp_id = text_id)
  )
  
  fb_merged %>%
    saveRDS(
      here(
        "data", "tidy", 
        paste0("fb_perspective_", chamber, "_", ymd(Sys.Date()), ".RDS")
      )
    )
}

df <- vec %>%
  map(
    ~ base::readRDS(
      max(list.files(here("data", "tidy"), pattern = "fb_perspective_", .x))
    )
  ) %>%
  bind_rows(., .id = "chamber")

save(df, file = here("output", "persp_final_results.Rda"))
