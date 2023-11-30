source(here::here("R", "utilities.R"))
options(digits = 3, scipen = 999)

# Load and wrangle data ========================================================
fb_unique <- read_rds(here("data", "tidy", "toxicity.RDS")) %>%
  ## Using max_ad_delivery_start_time, divide the 2020 election cycle
  ## into pre-primary, primary, and general election period
  mutate(
    election_cycle = case_when(
      ## Iowa caucuses were on Feb 3, but early voting began in Minnesota
      ## on Jan 17. Considering this, just use Jan 1, 2020 as the end of
      ## the pre-primary period
      max_ad_delivery_start_time < as.Date("2020-01-01") ~ "Pre-primary",
      max_ad_delivery_start_time >= as.Date("2020-01-01") &
        max_ad_delivery_start_time < as.Date("2020-04-09") ~ "Primary",
      ## Sanders dropped out on April 8, 2020 and endorsed Biden
      ## as the presumptive nominee
      max_ad_delivery_start_time >= as.Date("2020-04-09") ~ "General"
    ),
    election_cycle = factor(
      election_cycle,
      levels = c("Pre-primary", "Primary", "General"),
    )
  )

# Summary statistics: given general candidates, % of ads in each period? =======
prop(fb_unique, "election_cycle")
# Pre-primary     Primary     General
#        28.2        14.1        57.7

fb_unique %>%
  group_by(party, election_cycle) %>%
  summarise(n = n()) %>%
  mutate(prop = n / sum(n))
#   party      election_cycle     n  prop
#   <chr>      <fct>          <int> <dbl>
# 1 Democrat   Pre-primary    10856 0.326
# 2 Democrat   Primary         4549 0.137
# 3 Democrat   General        17894 0.537
# 4 Republican Pre-primary     5079 0.218
# 5 Republican Primary         3437 0.148
# 6 Republican General        14770 0.634

# Group by election cycle, split, and create summary figures ===================
cycle_list <- fb_unique %>%
  group_by(election_cycle) %>%
  group_split() %>%
  `names<-`({
    .
  } %>% map(~ .x$election_cycle[1]) %>% unlist()) %>%
  map(
    ## Within the groups, do what we did in perspective FB results script
    ~ summ_df_fxn(.x %>% group_by(party, financial, chamber))
  )

fb_perspective_plot(
  cycle_list$`Pre-primary`,
  xvar = "Toxic", se = "se_toxic",
  xlab = paste0(
    "Average Probability that Ad Text Would Be\n",
    "Perceived as Rude/Disrespectful/Toxic"
  )
)
ggsave(here("fig", "pre_primary_toxicity.pdf"), width = 6, height = 2.8)

fb_perspective_plot(
  cycle_list$Primary,
  xvar = "Toxic", se = "se_toxic",
  xlab = paste0(
    "Average Probability that Ad Text Would Be\n",
    "Perceived as Rude/Disrespectful/Toxic"
  )
)
ggsave(here("fig", "primary_toxicity.pdf"), width = 6, height = 2.8)

fb_perspective_plot(
  cycle_list$General,
  xvar = "Toxic", se = "se_toxic",
  xlab = paste0(
    "Average Probability that Ad Text Would Be\n",
    "Perceived as Rude/Disrespectful/Toxic"
  )
)
ggsave(here("fig", "general_toxicity.pdf"), width = 6, height = 2.8)

# Load ideology data, clean, and join ==========================================
## Only available for survivors of the 2020 election
dwnom <- read_csv(here("data", "raw", "data_congress", "HS117_members.csv")) %>%
  ## Delete all columns where there is only one unique value
  select(where(~ length(unique(.)) > 1)) %>%
  ## divide bioname into first and last name
  separate(
    bioname,
    into = c("last_name", "first_name"), sep = ",", remove = FALSE
  ) %>%
  ## If in the first name there is a parentheses, use the pattern only within
  ## e.g., Marion Michael (Mike) ---> leave just Mike
  mutate(
    first_name = str_replace_all(first_name, ".*\\((.*?)\\)$", "\\1")
  ) %>%
  ## Remove all non-alphanumeric characters from both and lowercase
  mutate(
    across(
      c(last_name, first_name),
      ~ str_remove_all(., "[^[:alnum:] ]") %>% tolower() %>% trimws()
    )
  ) %>%
  ## For first name, just leave the first word of the first name (drop middle)
  ## but if the first word is a single character, use the second instead
  mutate(
    first_name = str_split(first_name, " ") %>% 
      map_chr(
        ~ case_when(
          length(.x) == 1 ~ .x[1],
          length(.x) > 1 & nchar(.x[1]) == 1 ~ .x[2],
          TRUE ~ .x[1]
        )
      )
  ) %>%
  ## Create `candidate` where first and last name are combined by a space
  mutate(candidate = paste(first_name, last_name)) %>%
  select(bioname, first_name, last_name, candidate, everything()) %>%
  filter(chamber != "President") %>%
  ## Some manual fixes (welp turned out to be a lot more work for manual)
  mutate(
    candidate = case_when(
      candidate == "donald young" ~ "don young",
      candidate == "raúl grijalva" ~ "ral grijalva",
      candidate == "thomas ohalleran" ~ "tom ohalleran",
      candidate == "french hill" ~ "j french hill",
      candidate == "peter aguilar" ~ "pete aguilar",
      candidate == "rohit khanna" ~ "ro khanna",
      candidate == "andrew biggs" ~ "andy biggs",
      candidate == "alex mooney" ~ "alexander mooney",
      candidate == "abigail spanberger" ~ "abigail davis spanberger",
      candidate == "james panetta" ~ "jimmy panetta",
      candidate == "lucille roybalallard" ~ "lucille roybal allard",
      candidate == "joseph neguse" ~ "joe neguse",
      candidate == "vernon buchanan" ~ "vern buchanan",
      candidate == "ted deutch" ~ "theodore deutch",
      candidate == "valdez demings" ~ "val butler demings",
      candidate == "matthew gaetz" ~ "matt gaetz",
      candidate == "marjorie greene" ~ "marjorie taylor greene",
      candidate == "alfred lawson" ~ "al lawson jr",
      candidate == "william steube" ~ "w gregory steube",
      candidate == "buddy carter" ~ "earl buddy carter",
      candidate == "anderson ferguson" ~ "a drew ferguson iv",
      candidate == "sanford bishop" ~ "sanford bishop jr",
      candidate == "russell fulcher" ~ "russ fulcher",
      candidate == "brad schneider" ~ "bradley scott schneider",
      candidate == "jesús garcía" ~ "jess chuy garca",
      candidate == "james banks" ~ "jim banks",
      candidate == "gregory pence" ~ "greg pence",
      candidate == "marianette millermeeks" ~ "mariannette miller meeks",
      candidate == "dutch ruppersberger" ~ "c dutch ruppersberger",
      candidate == "frederick upton" ~ "fred upton",
      candidate == "john bergman" ~ "jack bergman",
      candidate == "thomas emmer" ~ "tom emmer",
      candidate == "angela craig" ~ "angie craig",
      candidate == "peter stauber" ~ "pete stauber",
      candidate == "samuel graves" ~ "sam graves",
      candidate == "matthew rosendale" ~ "matt rosendale",
      candidate == "donald bacon" ~ "don bacon",
      candidate == "tomaz malinowski" ~ "tom malinowski",
      candidate == "william pascrell" ~ "bill pascrell jr",
      candidate == "thomas reed" ~ "tom reed",
      candidate == "sean maloney" ~ "sean patrick maloney",
      candidate == "alexandria ocasiocortez" ~ "alexandria ocasio cortez",
      candidate == "nydia velázquez" ~ "nydia velzquez",
      candidate == "george butterfield" ~ "g butterfield",
      candidate == "theodore budd" ~ "ted budd",
      candidate == "timothy ryan" ~ "tim ryan",
      candidate == "matt cartwright" ~ "matthew alton cartwright",
      candidate == "mary scanlon" ~ "mary gay scanlon",
      candidate == "susan wild" ~ "susan ellis wild",
      candidate == "christina houlahan" ~ "chrissy houlahan",
      candidate == "dan meuser" ~ "daniel philip meuser",
      candidate == "frederick keller" ~ "fred keller",
      candidate == "michael doyle" ~ "michael doyle jr",
      candidate == "william timmons" ~ "william timmons iv",
      candidate == "duston johnson" ~ "dusty johnson",
      candidate == "james cooper" ~ "jim cooper",
      candidate == "stephen cohen" ~ "steve cohen",
      candidate == "timothy burchett" ~ "tim burchett",
      candidate == "daniel crenshaw" ~ "dan crenshaw",
      candidate == "randy weber" ~ "randy weber sr",
      candidate == "elizabeth fletcher" ~ "lizzie fletcher",
      candidate == "charles roy" ~ "chip roy",
      candidate == "blake moore" ~ "blake david moore",
      candidate == "gerry connolly" ~ "gerald connolly",
      candidate == "morgan griffith" ~ "h morgan griffith",
      candidate == "aston mceachin" ~ "a donald mceachin",
      candidate == "benjamin cline" ~ "ben cline",
      candidate == "robert bob good" ~ "bob good",
      candidate == "robert scott" ~ "robert bobby scott",
      candidate == "daniel newhouse" ~ "dan newhouse",
      candidate == "michael gallagher" ~ "mike gallagher",
      candidate == "shelley capito" ~ "shelley moore capito",
      candidate == "jack reed" ~ "john reed",
      candidate == "james inhofe" ~ "jim inhofe",
      candidate == "ben luján" ~ "ben lujan",
      candidate == "benjamin sasse" ~ "ben sasse",
      candidate == "cindy hydesmith" ~ "cindy hyde smith",
      candidate == "susan collins" ~ "susan margaret collins",
      candidate == "james risch" ~ "jim risch",
      candidate == "daniel sullivan" ~ "dan sullivan",
      TRUE ~ candidate
    )
  )

## Merge
fb_merged <- left_join(fb_unique, dwnom) %>%
  filter(!is.na(nominate_dim1)) %>%
  rename(Toxicity = toxicity, Candidate = candidate)

varlabel <- c(
  "partyRepublican" = "Republican",
  "financialDonor-targeting" = "Donor-targeting",
  "chamberSenate" = "Senate",
  "incINCUMBENT" = "Incumbent",
  "incOPEN" = "Open seat",
  "safety" = "Electoral safety",
  "gendermale" = "Male",
  "min_ad_delivery_start_time" = "First ad delivery date",
  "nominate_dim1" = "DW-NOMINATE (1st dim.)"
)

fit_se_cluster <- feols(
  Toxicity ~
    party + financial + chamber + inc + safety + gender +
    min_ad_delivery_start_time + state_po + nominate_dim1,
  fb_merged
)

summary(fit_se_cluster, cluster = ~Candidate)
etable(
  fit_se_cluster,
  cluster = "Candidate", replace = TRUE, dict = varlabel,
  drop = c("Constant", "state_po"),
  file = here("tab", "fit_cand_cluster_toxicity_dwnom_added.tex")
)

