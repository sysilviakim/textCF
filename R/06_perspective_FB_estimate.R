library(peRspective)
source(here::here("R", "utilities.R"))

# usethis::edit_r_environ()
# Sys.setenv(perspective_api_key = "AIzaSyBiSxBAIcVz_IvXPqF19TDkjFkjqrW8gLA")
# load(here("output", "fb_quanteda.Rda"))

# Load data ====================================================================
load(here("data", "tidy", "fb_unique.Rda"))

# Estimate House data with perspective API =====================================
batchHouse1 <- fbh %>%
  filter(persp_id %in% c(1:20000)) %>%
  prsp_stream(
    text = ad_creative_body,
    text_id = persp_id,
    languages = "en",
    score_model = c("IDENTITY_ATTACK", "TOXICITY", "OBSCENE")
  )

batchHouse2 <- fbh %>%
  filter(persp_id %in% c(20000:max(fbh$persp_id))) %>%
  prsp_stream(
    text = ad_creative_body,
    text_id = persp_id,
    languages = "en",
    score_model = c("IDENTITY_ATTACK", "TOXICITY", "OBSCENE")
  )

batch_combineHOUSE <- bind_rows(batchHouse1, batchHouse2)

fbHouse_Merged <- left_join(
  fbh,
  batch_combineHOUSE %>%
    dplyr::rename(persp_id = text_id)
)

fbHouse_Merged %>%
  saveRDS(here("data", "tidy", "fb_perspective_House.RDS"))

# Estimate Senate data =========================================================
fbs <- fb_unique$senate %>%
  mutate(persp_id = row_number())


batchSen1 <- fbs %>%
  filter(persp_id %in% c(1:10000)) %>%
  prsp_stream(
    text = ad_creative_body,
    text_id = persp_id,
    languages = "en",
    score_model = c("IDENTITY_ATTACK", "TOXICITY", "OBSCENE")
  )

batchSen2 <- fbs %>%
  filter(persp_id %in% c(10001:20000)) %>%
  prsp_stream(
    text = ad_creative_body,
    text_id = persp_id,
    languages = "en",
    score_model = c("IDENTITY_ATTACK", "TOXICITY", "OBSCENE")
  )

batchSen3 <- fbs %>%
  filter(persp_id %in% c(20001:max(fbs$persp_id))) %>%
  prsp_stream(
    text = ad_creative_body,
    text_id = persp_id,
    languages = "en",
    score_model = c("IDENTITY_ATTACK", "TOXICITY", "OBSCENE")
  )

batch_combineSENATE <- bind_rows(
  batchSen1,
  batchSen2,
  batchSen3
)

fbSenate_Merged <- left_join(
  fbs,
  batch_combineSENATE %>%
    dplyr::rename(persp_id = text_id)
)

fbSenate_Merged %>%
  saveRDS(here("data", "tidy", "fb_perspective_Senate.RDS"))
