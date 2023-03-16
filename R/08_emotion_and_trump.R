source(here::here("R", "utilities.R"))

# Load data ====================================================================
## Text is in "ad_creative_body"
load(here("data", "tidy", "fb_matched.Rda"))
load(here("data", "tidy", "fb_unique.Rda"))

# Adjust candidate labels so that nchar is the same ============================
## Define function, adjust later
temp_fxn <- function(x) {
  x %>%
    map(
      ~ .x %>%
        rowwise() %>%
        mutate(
          candidate = str_pad(
            simple_cap(tolower(candidate)),
            side = "left",
            width = fb_unique %>% map_dbl(~ max(nchar(.x$candidate))) %>% max()
          ),
          party = simple_cap(tolower(party))
        ) %>%
        ungroup() %>%
        ## For now
        filter(party != "Independent") %>%
        filter(!is.na(party)) %>%
        filter(party != "NANA") ## Accidental pasting of NA values
    )
}

# Top diverse ads or number of ads =============================================
## Note that candidate is the group-level marker, not page_name
## e.g., Luke Letlow For Congress != Luke Letlow, but same candidate
assert_that(
  length(unique(fb_unique$senate$page_name)) != ## 81
    length(unique(fb_unique$senate$candidate)) ## 70
)
assert_that(
  length(unique(fb_unique$house$page_name)) != ## 761
    length(unique(fb_unique$house$candidate)) ## 650
)

## Based on the number of diverse ads or total ads (not accounting for breadth)
fb_unique <- temp_fxn(fb_unique)

# Mention of Trump =============================================================
temp <- fb_unique %>%
  map_dfr(
    ~ .x %>%
      group_by(party, financial) %>%
      summarise(
        mean_trump = mean(word_trump),
        sd_trump = sd(word_trump),
        se_trump = sd_trump / sqrt(n())
      ),
    .id = "chamber"
  ) %>%
  party_factor(., outvar = "Party") %>%
  rowwise() %>%
  mutate(chamber = simple_cap(chamber)) %>%
  ungroup()

pdf(here("fig", "mention_trump_by_type_chamber.pdf"), width = 6, height = 2.8)
print(fb_mention_plot(
  temp,
  xvar = "mean_trump", se = "se_trump", xlab = "Mentions Trump"
))
dev.off()

## T-tests
t.test(
  fb_unique$senate %>%
    filter(party == "Republican" & financial == "Donor-targeting") %>%
    .$word_trump,
  fb_unique$senate %>%
    filter(party == "Republican" & financial == "Voter-targeting") %>%
    .$word_trump
)

# Emotionally charged rhetoric =================================================
fb_corpus_list <- list(
  ## Not a great practice; will try to fix later
  senate_rep_f = fb_unique$senate %>%
    filter(party == "Republican" & financial == "Donor-targeting") %>%
    corpus(., text_field = "ad_creative_body"),
  senate_rep_n = fb_unique$senate %>%
    filter(party == "Republican" & financial == "Voter-targeting") %>%
    corpus(., text_field = "ad_creative_body"),
  senate_dem_f = fb_unique$senate %>%
    filter(party == "Democrat" & financial == "Donor-targeting") %>%
    corpus(., text_field = "ad_creative_body"),
  senate_dem_n = fb_unique$senate %>%
    filter(party == "Democrat" & financial == "Voter-targeting") %>%
    corpus(., text_field = "ad_creative_body"),
  house_rep_f = fb_unique$house %>%
    filter(party == "Republican" & financial == "Donor-targeting") %>%
    corpus(., text_field = "ad_creative_body"),
  house_rep_n = fb_unique$house %>%
    filter(party == "Republican" & financial == "Voter-targeting") %>%
    corpus(., text_field = "ad_creative_body"),
  house_dem_f = fb_unique$house %>%
    filter(party == "Democrat" & financial == "Donor-targeting") %>%
    corpus(., text_field = "ad_creative_body"),
  house_dem_n = fb_unique$house %>%
    filter(party == "Democrat" & financial == "Voter-targeting") %>%
    corpus(., text_field = "ad_creative_body")
)

lexi_list <- fb_corpus_list %>%
  map(~ liwcalike(.x, dictionary = NRC))
save(lexi_list, file = here("output", "lexi_list.Rda"))

lexi_plots <- lexi_list %>%
  imap(
    ~ emotion_plot(
      .x,
      title = paste0(
        simple_cap(str_match_all(.y, "_(.*?)_")[[1]][1, 2]), ", ",
        ifelse(
          grepl("f", str_match_all(.y, "_(.*?)$")[[1]][1, 2]),
          "Donor-targeting", "Voter-targeting"
        )
      )
    )
  )

temp <- lexi_list %>%
  imap_dfr(~ .x %>% select(Dic, anger, disgust, fear), .id = "type") %>%
  mutate(
    chamber = case_when(
      grepl("senate", type) ~ "Senate", TRUE ~ "House"
    ),
    party = case_when(
      grepl("rep", type) ~ "Republican", TRUE ~ "Democrat"
    ),
    financial = case_when(
      grepl("_f", type) ~ "Donor-targeting", TRUE ~ "Voter-targeting"
    )
  ) %>%
  party_factor(., outvar = "type")

emo_temp <- temp %>%
  group_by(type, chamber) %>%
  summarise(
    mean_anger = mean(anger / Dic, na.rm = TRUE),
    mean_disgust = mean(disgust / Dic, na.rm = TRUE),
    mean_fear = mean(fear / Dic, na.rm = TRUE),
    se_anger = sd(anger / Dic, na.rm = TRUE) / sqrt(n()),
    se_disgust = sd(disgust / Dic, na.rm = TRUE) / sqrt(n()),
    se_fear = sd(fear / Dic, na.rm = TRUE) / sqrt(n())
  ) %>%
  ungroup()

pdf(here("fig", "anger_by_type_chamber.pdf"), width = 6, height = 2.8)
print(
  fb_mention_plot(
    emo_temp %>% rename(Party = type),
    xvar = "mean_anger", se = "se_anger", xlab = ""
  ) +
    scale_x_continuous(limits = c(0, 0.09), labels = scales::percent)
)
dev.off()

pdf(here("fig", "disgust_by_type_chamber.pdf"), width = 6, height = 2.8)
print(
  fb_mention_plot(
    emo_temp %>% rename(Party = type),
    xvar = "mean_disgust", se = "se_disgust", xlab = ""
  ) +
    scale_x_continuous(limits = c(0, 0.09), labels = scales::percent)
)
dev.off()

pdf(here("fig", "fear_by_type_chamber.pdf"), width = 6, height = 2.8)
print(
  fb_mention_plot(
    emo_temp %>% rename(Party = type),
    xvar = "mean_fear", se = "se_fear", xlab = ""
  ) +
    scale_x_continuous(limits = c(0, 0.09), labels = scales::percent)
)
dev.off()
