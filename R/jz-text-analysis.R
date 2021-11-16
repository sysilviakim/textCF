# Load the Corpora & DFMs
source("R/jz-text-text-prep-webisites.R")


###########################
# INPUTS: DICTIONARIES
###########################

# Read in the Moral foundations dictionary
MFD <- dictionary(file = "data/raw/dictionaries/mfd2.0.dic")

# Read in the Trolling dictionary:
troll <- read_csv(file = "data/raw/dictionaries/troll_and_divide/troll_and_divide_Glove_Expansion_Raters_phase_2.csv") %>%
  pull(Word) %>%
  list(troll = .) %>%
  dictionary()



###########################
# Apply dictionaries to DFMs
###########################


lookup_trolling <- dfm_lookup(dfm_congress, troll)
lookup_MFD <- dfm_lookup(dfm_congress, MFD)


###########################
# Export look-up tables
###########################

data_troll <- quanteda::convert(lookup_trolling,to="data.frame") %>%
  cbind(docvars(dfm_congress))

data_MFD <- quanteda::convert(lookup_MFD,to="data.frame") %>%
                      cbind(docvars(dfm_congress))


###########################
# Prepare charts
###########################

# Trolling
##########

data_troll %>% 
  group_by(party, chamber) %>%
  summarise(average_trolling_words = mean(troll)) %>%
  ggplot(aes(x=party, y = average_trolling_words, fill = party)) +
  geom_col(alpha=.8, width = .5) +
  facet_grid(~ chamber) +
  scale_fill_manual(values = c("midnightblue","darkgreen","darkred")) +
  labs(x = "", 
       y = "Average number of dict. words per document",
  title = "Occurence or words from the trolling-and-incivility dictionary
broken down by Party and chamber of US Congress",
  subtitle = "Corpus: Candidate websites")
ggsave("fig/dict_trolling_websites.png")

# Summary for the US House Only
data_troll %>% 
  mutate(House = str_detect(doc_id,"House")) %>%
  filter(House == T) %>%
  group_by(party) %>%
  summarise(average_trolling_words = mean(troll))

# Summary for the US Senate Only
data_troll %>% 
  mutate(Senate = str_detect(doc_id,"Senate")) %>%
  filter(Senate == T) %>%
  group_by(party) %>%
  summarise(average_trolling_words = mean(troll))



# MORAL FOUNDATIONS (MFD)
#########################

data_MFD %>%
  pivot_longer(cols = 2:11, 
               names_to = "moral_foundation", 
               values_to = "N") %>%
  group_by(party) %>%
  summarise(avg = mean(N))

data_MFD %>%
  pivot_longer(cols = 2:11, 
               names_to = "moral_foundation", 
               values_to = "N") %>%
  group_by(moral_foundation,party) %>%
  summarise(avg = mean(N))

data_MFD %>%
  pivot_longer(cols = 2:11, 
               names_to = "moral_foundation", 
               values_to = "N") %>%
  group_by(party, chamber) %>%
  summarise(avg = mean(N)) %>%
ggplot(aes(x=party, y = avg, fill = party)) +
  geom_col(alpha=.8, width = .5) +
  facet_grid(~ chamber) +
  scale_fill_manual(values = c("midnightblue","darkgreen","darkred")) +
  labs(x = "", 
       y = "Average number of dict. words per document",
       title = "Occurence or words from the Moral Foundations dictionary
broken down by Party and chamber of US Congress",
       subtitle = "Corpus: Candidate websites")
ggsave("fig/dict_MFD_websites.png")

data_MFD %>% 
  pivot_longer(cols = 2:11, 
               names_to = "moral_foundation", 
               values_to = "N") %>%
  filter(N >=1,
         party=="REPUBLICAN") %>%
  ggplot(aes(x= N)) +
  geom_histogram() +
  facet_wrap(~ moral_foundation) +
  labs(
       x = "Moral foundation words used on candidates websistes",
       y = "Word count",
       subtitle = "Corpus: US House and Senate Republicans") 
ggsave("fig/dict_MFD_by_dimension_websites_GOP.png")

data_MFD %>% 
  pivot_longer(cols = 2:11, 
               names_to = "moral_foundation", 
               values_to = "N") %>%
  filter(N >=1,
         party=="DEMOCRAT") %>%
  ggplot(aes(x= N)) +
  geom_histogram() +
  facet_wrap(~ moral_foundation) +
  labs(
    x = "Moral foundation words used on candidates websistes",
    y = "Word count",
    subtitle = "Corpus: US House and Senate Democrats") 
ggsave("fig/dict_MFD_by_dimension_websites_Democrats.png")

data_MFD %>% 
  mutate(Party = case_when(party %in% c("DEMOCRAT", "REPUBLICAN") ~ party,
                           party == "INDEPENDENT" ~ "DEMOCRAT")) %>%
  pivot_longer(cols = 2:11, 
               names_to = "moral_foundation", 
               values_to = "N") %>%
  # filter(N >=1) %>%
  filter(str_detect(moral_foundation,"author")) %>%
  ggplot(aes(x= N)) +
  geom_histogram() +
  facet_wrap(~ Party) +
  labs(title = "Words related to AUTHORITY",
       subtitle = "Moral Foundations Dictionary",
       x = "Number of texts with the given number of occurences",
       caption = "Angus King and Bernie Sanders, who caucuse with Democrats, are counted as Ds.")
ggsave("fig/dict_MFD_authority_by_party.png")
