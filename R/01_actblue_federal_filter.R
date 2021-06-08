source(here::here("R", "utilities.R"))
library(stringdist)

# Load ActBlue JS scrape results ===============================================
load(here("data/raw/2022/actblue/actblue_js_scraped.Rda"))

# Unused parts =================================================================
js_rest <- actblue_js_full$js_rest
post_donation_upsells <- actblue_js_full$post_donation_upsells %>% bind_rows()

# Federal candidates ===========================================================
df <- actblue_js_full %>%
  select(-js_rest, -eligibility_values_es, -post_donation_upsells) %>%
  bind_cols(., js_rest) %>%
  clean_names() %>%
  rename(id_main = id, kind_main = kind) %>%
  unnest(cols = c(entities), names_repair = "unique") %>%
  filter(kind == "candidate" & `federal?` == TRUE) %>%
  clean_names() %>%
  dedup() %>%
  select(
    -displayname_with_article,
    -contains("contribution_blurb_bottom"),
    -contains("eligibility_values"),
    -contains("_es$"),
    -contains("disclaimer_flags"),
    -contains("radio_amounts")
  ) %>%
  rowwise() %>%
  filter(!is.null(managing_entity)) %>%
  mutate(
    managing_id = managing_entity$id,
    managing_name = managing_entity$display_name,
    name_diff = stringdist(display_name, managing_name)
  ) %>%
  ungroup() %>%
  select(
    url, id, display_name, managing_id, managing_name, name_diff,
    display_pretty_location, everything()
  ) %>%
  ## name_diff is 5 or more
  select(-name_diff) %>%
  select(-managing_entity) %>%
  rename(id_x = id, display_name_x = display_name) %>%
  unnest(cols = c(brandings), names_repair = "unique") %>%
  clean_names() %>%
  dedup() %>%
  rename(
    id_brandings = id, display_name_brandings = display_name,
    id = id_x, display_name = display_name_x
  )

# Unnesting ====================================================================
temp <- nested_df(df) %>%
  clean_names()

## Twice to make sure (double- or triple-nested, sometimes)
temp <- nested_df(temp) %>%
  clean_names()

## Kick out aesthetics e.g., border, padding, background, font
temp <- temp %>%
  select(-contains("border_")) %>%
  select(-contains("padding_")) %>%
  select(-contains("text_")) %>%
  select(-contains("font")) %>%
  select(-contains("margin")) %>%
  select(-contains("color")) %>%
  select(-contains("alignment")) %>%
  select(-contains("letter_")) %>%
  select(-contains("space_")) %>%
  select(-contains("css")) %>%
  select(-contains("height"))

assert_that(
  temp %>%
    map_chr(class) %>%
    {
      which(. == "data.frame")
    } %>% length() == 0
)

# Entities vs. managing entities ===============================================
temp <- temp %>%
  filter(display_name == managing_name)

# Sanity checks ================================================================
## Must be political
assert_that(all(temp$political == TRUE))

## Is not charity
assert_that(all(temp$charity == FALSE))

## Not c4, i.e., 501(c)
assert_that(all(temp$c4 == FALSE))

## Donate is all TRUE
assert_that(all(temp$donate == TRUE))

## Drop unnecessary variables
actblue_federal <- temp %>%
  select(-federal, -political, -charity, -c4, -kind, -managing_name)

# More cleaning, including amounts converted to dollar =========================
actblue_federal <- actblue_federal %>%
  clean_names() %>%
  mutate(contribution_limit = contribution_limit / 100) %>%
  select(
    -contains("background_image_url"), -header_image_url,
    -acceptable_card_types, -ticket_types,
    everything()
  )

# Delete unnecessary phrases from candidate name ===============================
actblue_federal <- actblue_federal %>%
  filter(!grepl("2016|Marijuana Reform Party PAC", display_name)) %>%
  rowwise() %>%
  mutate(
    display_name = gsub(" for Congress$| - Federal$", "", display_name),
    display_name = gsub("\\(MA 04\\)| \\(\\)", "", display_name),
    display_name = gsub("^Friends of ", "", display_name),
    display_name = trimws(
      stringi::stri_trans_general(display_name, "latin-ascii")
    )
  )

save(actblue_federal, file = here("data/tidy/actblue_federal_2022.Rda"))
