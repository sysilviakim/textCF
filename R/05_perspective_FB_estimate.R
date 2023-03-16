library(peRspective)
source(here::here("R", "utilities.R"))
load(here("data", "tidy", "fb_unique.Rda"))
vec <- c(senate = "senate", house = "house")
batch_size <- 1e2

# Estimate House/Senate data with perspective API ==============================
for (chamber in vec) {
  df <- fb_unique[[chamber]] %>%
    mutate(persp_id = row_number()) %>%
    select(
      persp_id, candidate, fb_ad_library_id, ad_creative_body,
      page_name, ad_creative_body, ad_creative_link_caption
    )

  ## Batch-loop
  batch_list <- vector("list", (nrow(df) %/% batch_size) + 1)
  for (i in seq(nrow(df) %/% batch_size + 1)) {
    batch_list[[i]] <- df %>%
      filter(
        persp_id %in%
          seq(batch_size * (i - 1) + 1, min(batch_size * i, nrow(df)))
      ) %>%
      prsp_stream(
        text = ad_creative_body,
        text_id = persp_id,
        languages = "en",
        score_model = c("TOXICITY", "OBSCENE")
      )

    print(paste0(i, "-th batch finished for ", chamber, "."))
    Sys.sleep(30) ## prevent HTTP 429 resource exhausted error

    save(
      batch_list,
      file = here(
        "output",
        paste0(
          "batch_", chamber, "_list_", format(Sys.Date(), "%Y%m%d"), ".Rda"
        )
      )
    )
  }
  print("All batches finished!! ==============================================")

  temp <- batch_list %>%
    bind_rows() %>%
    rename(persp_id = text_id)
  assert_that(nrow(df) == nrow(temp))
  fb_merged <- left_join(df, temp) %>% clean_names()

  fb_merged %>%
    saveRDS(
      here(
        "data", "tidy",
        paste0(
          "fb_perspective_", chamber, "_", format(Sys.Date(), "%Y%m%d"), ".RDS"
        )
      )
    )
}

## Reads in the most recent rendering
df <- vec %>%
  map(
    ~ base::readRDS(
      max(
        list.files(
          here("data", "tidy"),
          pattern = paste0("fb_perspective_", .x),
          full.names = TRUE
        )
      )
    )
  ) %>%
  bind_rows(., .id = "chamber")

## Without metadata; just bare minimum for perspective matching
save(
  df,
  file = here(
    "output",
    paste0("persp_final_results_", format(Sys.Date(), "%Y%m%d"), ".Rda")
  )
)
