# Load the Corpora & DFMs
source("R/jz-text-text-prep-websites.R")


###########################
# Apply dictionaries to DFMs
###########################

lookup_troll <- dfm_lookup(dfm_congress, troll)
lookup_moral <- dfm_lookup(dfm_congress, moral)


###########################
# Export look-up tables
###########################

data_troll <- quanteda::convert(lookup_troll, to = "data.frame") %>%
  cbind(docvars(dfm_congress))

data_moral <- quanteda::convert(lookup_moral, to = "data.frame") %>%
  cbind(docvars(dfm_congress))


###########################
# Prepare charts
###########################

# Trolling
##########

data_troll %>%
  group_by(party, chamber) %>%
  summarise(average_troll_words = mean(troll)) %>%
  ggplot(aes(x = party, y = average_troll_words, fill = party)) +
  geom_col(alpha = .8, width = .5) +
  facet_grid(~chamber) +
  scale_fill_manual(values = c("midnightblue", "darkgreen", "darkred")) +
  labs(
    x = "",
    y = "Average number of dict. words per document",
    title = paste0(
      "Occurence or words from the troll-and-incivility dictionary ", 
      "broken down by Party and chamber of US Congress"
    ),
    subtitle = "Corpus: Candidate websites"
  )
ggsave("fig/dict_troll_websites.png")

# Summary for the US House Only
data_troll %>%
  mutate(House = str_detect(doc_id, "House")) %>%
  filter(House == T) %>%
  group_by(party) %>%
  summarise(average_troll_words = mean(troll))

# Summary for the US Senate Only
data_troll %>%
  mutate(Senate = str_detect(doc_id, "Senate")) %>%
  filter(Senate == T) %>%
  group_by(party) %>%
  summarise(average_troll_words = mean(troll))



# MORAL FOUNDATIONS (moral)
#########################

data_moral %>%
  pivot_longer(
    cols = 2:11,
    names_to = "moral_foundation",
    values_to = "N"
  ) %>%
  group_by(party) %>%
  summarise(avg = mean(N))

data_moral %>%
  pivot_longer(
    cols = 2:11,
    names_to = "moral_foundation",
    values_to = "N"
  ) %>%
  group_by(moral_foundation, party) %>%
  summarise(avg = mean(N))

data_moral %>%
  pivot_longer(
    cols = 2:11,
    names_to = "moral_foundation",
    values_to = "N"
  ) %>%
  group_by(party, chamber) %>%
  summarise(avg = mean(N)) %>%
  ggplot(aes(x = party, y = avg, fill = party)) +
  geom_col(alpha = .8, width = .5) +
  facet_grid(~chamber) +
  scale_fill_manual(values = c("midnightblue", "darkgreen", "darkred")) +
  labs(
    x = "",
    y = "Average number of dict. words per document",
    title = "Occurence or words from the Moral Foundations dictionary
broken down by Party and chamber of US Congress",
    subtitle = "Corpus: Candidate websites"
  )
ggsave("fig/dict_moral_websites.png")

data_moral %>%
  pivot_longer(
    cols = 2:11,
    names_to = "moral_foundation",
    values_to = "N"
  ) %>%
  filter(
    N >= 1,
    party == "REPUBLICAN"
  ) %>%
  ggplot(aes(x = N)) +
  geom_histogram() +
  facet_wrap(~moral_foundation) +
  labs(
    x = "Moral foundation words used on candidates websistes",
    y = "Word count",
    subtitle = "Corpus: US House and Senate Republicans"
  )
ggsave("fig/dict_moral_by_dimension_websites_GOP.png")

data_moral %>%
  pivot_longer(
    cols = 2:11,
    names_to = "moral_foundation",
    values_to = "N"
  ) %>%
  filter(
    N >= 1,
    party == "DEMOCRAT"
  ) %>%
  ggplot(aes(x = N)) +
  geom_histogram() +
  facet_wrap(~moral_foundation) +
  labs(
    x = "Moral foundation words used on candidates websistes",
    y = "Word count",
    subtitle = "Corpus: US House and Senate Democrats"
  )
ggsave("fig/dict_moral_by_dimension_websites_Democrats.png")

data_moral %>%
  mutate(
    Party = case_when(
      party %in% c("DEMOCRAT", "REPUBLICAN") ~ party,
      party == "INDEPENDENT" ~ "DEMOCRAT"
    )
  ) %>%
  pivot_longer(
    cols = 2:11,
    names_to = "moral_foundation",
    values_to = "N"
  ) %>%
  # filter(N >=1) %>%
  filter(str_detect(moral_foundation, "author")) %>%
  ggplot(aes(x = N)) +
  geom_histogram() +
  facet_wrap(~Party) +
  labs(
    title = "Words related to AUTHORITY",
    subtitle = "Moral Foundations Dictionary",
    x = "Number of texts with the given number of occurences",
    caption = paste0(
      "Angus King and Bernie Sanders, who caucuse with Democrats,",
      "are counted as Ds."
    )
  )
ggsave("fig/dict_moral_authority_by_party.png")
