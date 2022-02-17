library(tidyverse)


####################################
# THIS IS A VERY PRELIMINIARY FILE !!!
####################################



fbHouse_Merged <- read_rds("data/tidy/fb_perspective_House.RDS")
fbSenate_Merged <- read_rds("data/tidy/fb_perspective_Senate.RDS")


# !!!! warning
# Some SENATORS HAVE MISSING PID
# So Senate data is super-preliminary
# !!!
bind_rows(
  fbHouse_Merged %>% mutate(chamber = "House"),
  fbSenate_Merged %>% mutate(chamber = "Senate")
  ) %>%
  group_by(party,financial,chamber) %>%
  dplyr::summarise(`Toxic` = mean(TOXICITY),
                   n = n(),
                   sd = sd(TOXICITY,na.rm=T),
                   se = sd / sqrt(n)) %>%
  filter(party %in% c("DEMOCRAT","REPUBLICAN")) %>%
  mutate(type = glue("{party} {financial} type")) %>%
  ggplot(aes(y = fct_reorder(type,Toxic),
             x = Toxic,
             fill = type,
             color = type,
             xmax=Toxic + 1.96*se, 
             xmin=Toxic - 1.96*se)) +
  geom_pointrange(size=1) +
  facet_wrap(~chamber) +
  theme_bw() +
  theme(legend.position = "none") +
  scale_color_brewer(type = "qual", palette = 2, direction = -1) +
  labs(y = "",
       x = "Average probability text would be perceived as rude/disrespectful/toxic",
       title = "Level of language toxicity by Facebook ad type")
ggsave("fig/fig1_ad_means_by_type_2chambers.pdf",width = 8,height = 6)

fbHouse_Merged %>%
  group_by(type) %>%
  dplyr::summarise(Toxic = mean(TOXICITY), 
                   N = n(),
                    sd = sd(TOXICITY,na.rm=T),
                    se = sd / sqrt(N)) %>%
  dplyr::arrange(-Toxic) %>%
  filter(type != "") %>%
  ggplot(aes(y = fct_reorder(type,Toxic),
             x = Toxic,
             fill = type,
             color = type,
             xmax=Toxic + 1.96*se, 
             xmin=Toxic - 1.96*se)) +
  geom_pointrange() +
  theme_bw() +
  theme(legend.position = "none") +
  labs(y = "",
       x = "Average probability text would be perceived as rude/disrespectful/toxic",
       title = "Level of language toxicity by Facebook ad type",
       subtitle = "US House candidates")
ggsave("fig/fig1_ad_means_by_type.pdf",width = 8,height = 6)


fbHouse_Merged %>%
  group_by(party,financial) %>%
  dplyr::summarise(`Toxic` = mean(TOXICITY),
                   n = n(),
                   sd = sd(TOXICITY,na.rm=T),
                   se = sd / sqrt(n)) %>%
  filter(party %in% c("DEMOCRAT","REPUBLICAN")) %>%
  mutate(type = glue("{party} {financial} type")) %>%
  ggplot(aes(y = fct_reorder(type,Toxic),
             x = Toxic,
             fill = type,
             color = type,
             xmax=Toxic + 1.96*se, 
             xmin=Toxic - 1.96*se)) +
  geom_pointrange(size=1) +
  theme_bw() +
  theme(legend.position = "none") +
  scale_color_brewer(type = "qual", palette = 2, direction = -1) +
  labs(y = "",
       x = "Average probability text would be perceived as rude/disrespectful/toxic",
       title = "Level of language toxicity by Facebook ad type",
       subtitle = "US House candidates")
ggsave("fig/fig1B_ad_means_by_type.pdf",width = 8,height = 6)



candidate_Tox <- fbHouse_Merged %>%
  group_by(candidate,party) %>%
  dplyr::summarise(`Avg. prob. toxic` = mean(TOXICITY), 
                   N = n())

toxDEMcand <- candidate_Tox %>%
  filter(party=="DEMOCRAT") %>%
  filter(N >=20) %>%
  ggplot(aes(x=`Avg. prob. toxic`)) +
  geom_histogram() +
  labs(y = "Candidates", 
       x="Average usage of toxic language",
       title = "Democratic Candidates") +
  geom_vline(aes(xintercept = mean(`Avg. prob. toxic`)),col='blue',size=1.5) +
  geom_text(
    aes(x = mean(`Avg. prob. toxic`) +.03, 
        y = 25, 
        label = paste0("Mean:",mean(`Avg. prob. toxic`) %>% round(3)))) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

toxREPcand <- candidate_Tox %>%
  filter(party=="REPUBLICAN") %>%
  filter(N >=20) %>%
  ggplot(aes(x=`Avg. prob. toxic`)) +
  geom_histogram() +
  labs(y = "Candidates",
       x="Average usage of toxic language",
       title = "Republican Candidates") +
  geom_vline(aes(xintercept = mean(`Avg. prob. toxic`)),col='red',size=1.5) +
  geom_text(
    aes(x = mean(`Avg. prob. toxic`) +.05, 
        y = 25, 
        label = paste0("Mean:",mean(`Avg. prob. toxic`) %>% round(3)))) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylim(c(0,30))

ggarrange(toxDEMcand,toxREPcand) %>%
  annotate_figure(
    top = text_grob("Distribution of candidate-level average ad toxicity (US House)", 
                    face = "bold", size = 14))
ggsave("fig/fig2_house_candidate_averages.pdf",width = 8, height = 5)



candidate_Tox %>%
  filter(N >=20) %>%
  filter(party %in% c("DEMOCRAT","REPUBLICAN")) %>%
  ggplot(aes(y = party,
             x= `Avg. prob. toxic`,
             color = party,
             label = candidate)) +
  geom_boxplot() +
  geom_jitter(height = .25) + 
  scale_color_brewer(palette = "Set1",direction = -1) +
  labs(y="")
ggsave("fig/fig2B_house_candidate_averages.pdf",width = 8, height = 4)


# Keeping a type if N>100 for any party group:
keepPostType <- fbHouse_Merged %>% 
  dplyr::count(party,type) %>% 
  filter(n >= 100, type != "") %>%
  mutate(post = glue("{type} posted by {party}")) %>%
  pull(post)

fbHouse_Merged %>% 
  # filter(type %in% c("ActBlue","Winred","Non-financial",
  #                    "Personal Contribution Link")) %>%
  mutate(post = glue("{type} posted by {party}")) %>%
  filter(post %in% keepPostType) %>%
  ggplot(aes(y = post,
             x = TOXICITY,
             fill = post)) +
  ggridges::geom_density_ridges() +
  xlim(c(0,.3)) +
  theme(legend.position = "none") +
  labs(y = "",
       x = "Prob. text would be perceived as rude/disrespectful/toxic",
       title = "US House",
       subtitle = "Type shown if at least 100 posts of a given type exists")
ggsave("fig/ridges_by_type.pdf",width = 8,height = 6)


# EXAMPLES OF TOXIC DEM ADS
# -------------------------
fbHouse_Merged %>%
  filter(TOXICITY>=.75) %>%
  filter(party=="DEMOCRAT") %>%
  pull(ad_creative_body)

# EXAMPLES OF TOXIC REP ADS
# -------------------------
fbHouse_Merged %>%
  filter(TOXICITY>=.75) %>%
  filter(party=="REPUBLICAN") %>%
  pull(ad_creative_body)




fbHouse_Merged %>% 
  ggplot(aes(x=TOXICITY)) +
  geom_histogram() +
  geom_vline(aes(xintercept = mean(TOXICITY)),col='red',size=1.5)

tox_dem <- fbHouse_Merged %>% 
  filter(party=="DEMOCRAT") %>%
  ggplot(aes(x=TOXICITY)) +
  geom_histogram() +
  labs(x="", y = "Number of ads",
       title = "Democrats") +
  geom_vline(aes(xintercept = mean(TOXICITY)),col='blue',size=1.5) +
  geom_text(
    aes(x = mean(TOXICITY) +.2, 
        y = 1000, 
        label = paste0("Mean:",mean(TOXICITY) %>% round(2))))

tox_rep <- fbHouse_Merged %>% 
  filter(party=="REPUBLICAN") %>%
  ggplot(aes(x=TOXICITY)) +
  geom_histogram() +
  labs(x="", y = "Number of ads",
       title = "Republicans") +
  geom_vline(aes(xintercept = mean(TOXICITY)),col='red',size=1.5) +
  geom_text(
    aes(x = mean(TOXICITY) +.15, 
        y = 1000, 
        label = paste0("Mean:",mean(TOXICITY) %>% round(2))))


ggarrange(tox_rep,tox_dem) %>%
  annotate_figure(
    top = text_grob("Distribution of toxicity probabilities in Facebook ads", 
                    face = "bold", size = 14),
    bottom = text_grob("Prob(Ad text is toxic)"))
ggsave("fig/SI_ad_level_toxicity.pdf",width = 8, height = 5)


######################
#### Initial ANALYSIS
######################



fbHouse_Merged %>%
  group_by(party) %>%
  dplyr::summarise(`Toxic` = mean(TOXICITY), 
                   `Identity attack` = mean(IDENTITY_ATTACK), 
                   `Obscene` = mean(IDENTITY_ATTACK), 
                   `Number of ads` = n()) 

fbHouse_Merged %>%
  group_by(type) %>%
  dplyr::summarise(`Toxic` = mean(TOXICITY), 
                   `Identity attack` = mean(IDENTITY_ATTACK), 
                   `Obscene` = mean(IDENTITY_ATTACK), 
                   `Number of ads` = n()) %>%
  dplyr::arrange(-Toxic) %>%
  filter(type != "")



fbHouse_Merged %>% group_by(party,type) %>%
  dplyr::summarise(`Toxic` = mean(TOXICITY), 
                   #`Identity attack` = mean(IDENTITY_ATTACK), 
                   `Obscene` = mean(IDENTITY_ATTACK), 
                   N = n()) %>%
  filter(N >= 100, type != "") %>%
  mutate(post = glue("{type} posted by {party}")) %>%
  pivot_longer(cols = c(Toxic,`Obscene`)) %>%
  ggplot(aes(y = post,
             x = value,
             color = name)) +
  geom_point() +
  facet_grid(~name)


temp0notext <- candidate_Tox %>%
  filter(N >=10) %>%
  filter(party %in% c("DEMOCRAT","REPUBLICAN")) %>%
  ggplot(aes(y = party,
             x= `Avg. prob. toxic`,
             color = party,
             label = candidate)) +
  geom_jitter() +
  scale_color_brewer(palette = "Set1",direction = -1) +
  labs(y="")
temp1 <- candidate_Tox %>%
  filter(N >=10) %>%
  filter(party %in% c("DEMOCRAT","REPUBLICAN")) %>%
  ggplot(aes(y = party,
             x= `Avg. prob. toxic`,
             color = party,
             label = candidate)) +
  geom_jitter() +
  ggrepel::geom_text_repel(max.overlaps=100) +
  scale_color_brewer(palette = "Set1",direction = -1) +
  labs(y="")
ggarrange(temp0notext,temp1,ncol=1)


fbHouse_Merged %>%
    group_by(candidate,party) %>%
    dplyr::summarise(`Avg. prob. toxic` = mean(TOXICITY), 
                     N = n()) %>%
  dplyr::arrange(-`Avg. prob. toxic`) %>%
  filter(str_detect(candidate,"GREEN"))

