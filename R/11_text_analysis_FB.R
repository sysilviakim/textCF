source(here::here("R", "utilities.R"))
library(seededlda)
library(stm)

# Load data ====================================================================
## Text is in "ad_creative_body"
load(here("data", "tidy", "fb_matched.Rda"))
load(here("data", "tidy", "fb_unique.Rda"))
load(here("output", "fb_quanteda.Rda"))

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
  length(unique(fb_unique$senate$page_name)) != ## 79
    length(unique(fb_unique$senate$candidate)) ## 69
)
assert_that(
  length(unique(fb_unique$house$page_name)) != ## 767
    length(unique(fb_unique$house$candidate)) ## 652
)

## Based on the number of diverse ads or total ads (not accounting for breadth)
fb_unique <- temp_fxn(fb_unique)

# Specific keywords ============================================================
## By type ---------------------------------------------------------------------
temp <- fb_unique %>%
  map_dfr(
    ~ .x %>%
      group_by(party, financial) %>%
      summarise(
        mean_trump = mean(word_trump), 
        sd_trump = sd(word_trump), 
        se_trump = sd_trump / sqrt(n()),
        mean_covid = mean(word_covid), 
        sd_covid = sd(word_covid), 
        se_covid = sd_covid / sqrt(n())
      ),
    .id = "chamber"
  ) %>%
  party_factor(., outvar = "Party") %>%
  rowwise() %>%
  mutate(chamber = simple_cap(chamber)) %>%
  ungroup()

pdf(here("fig", "mention_trump_by_type_chamber.pdf"), width = 6, height = 2.8)
print(fb_mention_plot(
  temp, xvar = "mean_trump", se = "se_trump", xlab = "Mentions Trump"
))
dev.off()

pdf(here("fig", "mention_covid_by_type_chamber.pdf"), width = 6, height = 2.8)
print(fb_mention_plot(
  temp, xvar = "mean_covid", se = "se_covid", xlab = "Mentions COVID-19"
))
dev.off()

## T-tests
t.test(
  fb_unique$senate %>% 
    filter(party == "Republican" & financial == "Financial") %>%
    .$word_trump,
  fb_unique$senate %>% 
    filter(party == "Republican" & financial == "Non-Financial") %>%
    .$word_trump
)

## Who mentions them the most? -------------------------------------------------
p <- top_list <- list()
for (topn in c(10, 20, 30, 1000)) {
  top_unique <- fb_unique %>% map(~ tally_by_cand(.x, lim = topn))
  top_all <- fb_matched %>%
    map(~ tally_by_cand(.x, lim = topn)) %>%
    temp_fxn()
  top_all <- temp_fxn(top_all)

  temp <- cross2(c("trump", "covid"), c("unique", "all")) %>%
    set_names(., nm = {
      .
    } %>% map_chr(~ paste(.x, collapse = "_"))) %>%
    imap(
      ~ {
        var <- paste0("word_", .x[[1]])
        list(
          freq = top_freq(
            l1 = fb_unique,
            l2 = get(paste0("top_", .x[[2]]), envir = .GlobalEnv),
            var = var
          ) %>%
            map(
              ~ .x %>%
                mutate(
                  ## There must be a smarter way :)
                  candidate = gsub("-c", "-C", candidate),
                  candidate = gsub("Laturner", "LaTurner", candidate),
                  candidate = gsub("Mcsally", "McSally", candidate),
                  candidate = gsub("Mcg", "McG", candidate),
                  candidate = gsub("Mcc", "McC", candidate),
                  candidate = gsub("Mcmorris", "McMorris", candidate),
                  candidate = gsub("Mcadams", "McAdams", candidate),
                  candidate = gsub("Iii", "III", candidate),
                  candidate = gsub("O¬íhalleran", "O'Halleran", candidate),
                  candidate = gsub("-n", "-N", candidate),
                  candidate = gsub("-g", "-G", candidate),
                  candidate = gsub("-s", "-S", candidate),
                  candidate = gsub("-p", "-P", candidate)
                ) %>%
                group_by(candidate) %>%
                mutate(
                  total = sum(n),
                  perc = n / total,
                  perc1 = perc[!!as.name(var) == 1]
                ) %>%
                ungroup()
            ),
          type = .x[[2]],
          var = var,
          lab0 = case_when(
            .x[[1]] == "trump" ~ "Does Not Mention Trump",
            .x[[1]] == "covid" ~ "Does Not Mention COVID-19",
            .x[[1]] == "chinese" ~ "Does Not Mention China"
          ),
          lab1 = case_when(
            .x[[1]] == "trump" ~ "Mentions Trump",
            .x[[1]] == "covid" ~ "Mentions COVID-19",
            .x[[1]] == "chinese" ~ "Mentions China"
          )
        )
      }
    )
  top_list[[paste0("top_", topn)]] <- temp

  ## Visualize -----------------------------------------------------------------
  ## Needs labels by state/district
  ## Number of ads
  p[[paste0("top_", topn)]][["no"]] <- temp %>%
    map(
      ~ list(
        senate = top_freq_plot(
          x = .x, chamber = "senate", top = topn, scales = "free_y",
          height = ifelse(topn == 10, 3.5, 5.2)
        ),
        house = top_freq_plot(
          x = .x, chamber = "house", top = topn, scales = "free_y",
          height = ifelse(topn == 10, 3.5, 5.2)
        )
      )
    )

  ## Percentages
  p[[paste0("top_", topn)]][["perc"]] <- temp %>%
    map(
      ~ list(
        senate = top_freq_plot(
          x = .x, chamber = "senate", top = topn, scales = "free_y",
          height = ifelse(topn == 10, 3.5, 5.2), fxn = "perc"
        ),
        house = top_freq_plot(
          x = .x, chamber = "house", top = topn, scales = "free_y",
          height = ifelse(topn == 10, 3.5, 5.2), fxn = "perc"
        )
      )
    )
}
save(top_list, file = here("output", "word_top_list.Rda"))
save(p, file = here("output", "word_top_list_figs.Rda"))

## Summary statistics --------------------------------------------------------
top_list %>%
  map_dfr(
    function(x) {
      x %>%
        map_dfr(
          ~ bind_rows(
            .x$freq$senate %>%
              filter(!!as.name(.x$var) == 1) %>%
              group_by(party) %>%
              summarise(perc = mean(perc)) %>%
              mutate(chamber = "Senate"),
            .x$freq$house %>%
              filter(!!as.name(.x$var) == 1) %>%
              group_by(party) %>%
              summarise(perc = mean(perc)) %>%
              mutate(chamber = "House")
          ),
          .id = "case"
        )
    },
    .id = "topn"
  ) %>%
  filter(party != "NANA") %>%
  filter(grepl("unique", case)) %>%
  filter(grepl("1000", topn))

# # A tibble: 12 x 5
# topn     case           party          perc chamber
# <chr>    <chr>          <chr>         <dbl> <chr>
#   1 top_1000 trump_unique   Democrat   0.130    Senate
# 2 top_1000 trump_unique   Republican 0.205    Senate
# 3 top_1000 trump_unique   Democrat   0.151    House
# 4 top_1000 trump_unique   Republican 0.158    House
# 5 top_1000 covid_unique   Democrat   0.0388   Senate
# 6 top_1000 covid_unique   Republican 0.0466   Senate
# 7 top_1000 covid_unique   Democrat   0.0566   House
# 8 top_1000 covid_unique   Republican 0.0416   House

# Topic modeling ===============================================================
temp <- dfm_FB_ad %>%
  dfm_trim(
    ## keep only the top 5% of the most frequent features (min_termfreq = 0.8)
    ## that appear in less than 10% of all documents (max_docfreq = 0.1)
    min_termfreq = 0.8, termfreq_type = "quantile", docfreq_type = "prop",
    ## Removing features that are rare: appearing in less than 10% of all docs
    ## Removing features that are ubiquitous: appearing in more than 90% of docs
    min_docfreq = 0.1, max_docfreq = 0.95
  )

tmod_lda <- textmodel_lda(temp, k = 10)
save(tmod_lda, file = here("output", "tmod_lda_fb_all.Rda"))
terms(tmod_lda, 10)

unique_docvars <- docvars(dfm_FB_ad) %>%
  select(financial, party, chamber) %>%
  dedup() %>%
  filter(!is.na(party) & party != "INDEPENDENT") %>%
  arrange(chamber, party, financial)

## Subgroups
tmod_lda_list <- list()
for (i in seq(nrow(unique_docvars))) {
  subtemp <- dfm_subset(
    temp,
    party == unique_docvars$party[i] &
      financial == unique_docvars$financial[i] &
      chamber == unique_docvars$chamber[i]
  )
  tmod_lda_list[[paste0(
    tolower(unique_docvars$party[i]),
    "_", tolower(unique_docvars$financial[i]),
    "_", tolower(unique_docvars$chamber[i])
  )]] <- tmod_lda <- textmodel_lda(subtemp, k = 10)
  save(
    tmod_lda,
    file = here(
      "output",
      paste0(
        "tmod_lda_fb_", tolower(unique_docvars$party[i]),
        "_", tolower(unique_docvars$financial[i]),
        "_", tolower(unique_docvars$chamber[i]), "_", ".Rda"
      )
    )
  )
}
save(tmod_lda_list, file = here("output", "tmod_lda_subgroups_all.Rda"))

tmod_lda_list %>%
  map(~ terms(.x, 10))

## Sum is always 1, so stacked barchart is sufficient
tidy_topics_list <- tmod_lda_list %>%
  map(~ as_tibble(.x$theta, rownames = "doc_id"))

p <- tidy_topics_list[[1]] %>%
  arrange(
    desc(topic1), desc(topic2), desc(topic3), desc(topic4), desc(topic5)
  ) %>%
  mutate(
    id = paste0(
      "id_", str_pad(as.character(row_number()), pad = "0", width = 5)
    ),
    id = factor(id)
  ) %>%
  select(-doc_id) %>%
  pivot_longer(cols = topic1:topic5, names_to = "topic", values_to = "prob") %>%
  mutate(topic = factor(topic, levels = paste0("topic", seq(5)))) %>%
  ggplot(., aes(x = id, y = prob, fill = fct_rev(topic))) +
  geom_bar(position = "stack", stat = "identity")

pdf_default(p) +
  theme(axis.text.x = element_blank()) +
  scale_fill_viridis_d(direction = -1)

# Trolling/moral foundation words ==============================================
lookup_troll <- dfm_lookup(dfm_FB_ad, troll)
lookup_moral <- dfm_lookup(dfm_FB_ad, moral)

data_troll <- convert(lookup_troll, to = "data.frame") %>%
  cbind(docvars(dfm_FB_ad))
data_moral <- convert(lookup_moral, to = "data.frame") %>%
  cbind(docvars(dfm_FB_ad))
data_moral_pivoted <- data_moral %>%
  pivot_longer(
    cols = c(contains("vice"), contains("virtue")),
    names_to = "moral_foundation",
    values_to = "N"
  )

p1 <- dict_plot(data_troll)
p2 <- data_moral_pivoted %>% dict_plot(., var = "N")

pdf(here("fig", "fb_troll_by_party_chamber.pdf"), width = 6, height = 2.8)
print(plot_nolegend(p1) + scale_x_continuous(limits = c(0, 2)))
dev.off()

pdf(here("fig", "fb_moral_by_party_chamber.pdf"), width = 6, height = 2.8)
print(plot_nolegend(p2) + scale_x_continuous(limits = c(0, 2)))
dev.off()

# Emotionally charged rhetoric =================================================
fb_corpus_list <- list(
  ## Not a great practice; will try to fix later
  senate_rep_f = fb_unique$senate %>%
    filter(party == "Republican" & financial == "Financial") %>%
    corpus(., text_field = "ad_creative_body"),
  senate_rep_n = fb_unique$senate %>%
    filter(party == "Republican" & financial == "Non-Financial") %>%
    corpus(., text_field = "ad_creative_body"),
  senate_dem_f = fb_unique$senate %>%
    filter(party == "Democrat" & financial == "Financial") %>%
    corpus(., text_field = "ad_creative_body"),
  senate_dem_n = fb_unique$senate %>%
    filter(party == "Democrat" & financial == "Non-Financial") %>%
    corpus(., text_field = "ad_creative_body"),
  house_rep_f = fb_unique$house %>%
    filter(party == "Republican" & financial == "Financial") %>%
    corpus(., text_field = "ad_creative_body"),
  house_rep_n = fb_unique$house %>%
    filter(party == "Republican" & financial == "Non-Financial") %>%
    corpus(., text_field = "ad_creative_body"),
  house_dem_f = fb_unique$house %>%
    filter(party == "Democrat" & financial == "Financial") %>%
    corpus(., text_field = "ad_creative_body"),
  house_dem_n = fb_unique$house %>%
    filter(party == "Democrat" & financial == "Non-Financial") %>%
    corpus(., text_field = "ad_creative_body")
)

lexi_list <- fb_corpus_list %>%
  map(~ liwcalike(.x, dictionary = NRC))

lexi_plots <- lexi_list %>%
  imap(
    ~ emotion_plot(
      .x,
      title = paste0(
        simple_cap(str_match_all(.y, "_(.*?)_")[[1]][1, 2]), ", ",
        ifelse(
          grepl("f", str_match_all(.y, "_(.*?)$")[[1]][1, 2]),
          "Financial", "Non-financial"
        )
      )
    )
  )

## Not very visibly distinguishable
pdf(here("fig", "emotion_fb_senate.pdf"), width = 6, height = 6)
Kmisc::grid_arrange_shared_legend(list = lexi_plots[1:4], ncol = 2, nrow = 2)
dev.off()

pdf(here("fig", "emotion_fb_house.pdf"), width = 6, height = 6)
Kmisc::grid_arrange_shared_legend(list = lexi_plots[5:8], ncol = 2, nrow = 2)
dev.off()

## Stacked percentage bar plots? Meh.
pdf(here("fig", "emotion_fb_senate_barplot.pdf"), width = 6, height = 4)
print(emotion_barplot(lexi_list[1:4]))
dev.off()

pdf(here("fig", "emotion_fb_house_barplot.pdf"), width = 6, height = 4)
print(emotion_barplot(lexi_list[5:8]))
dev.off()
