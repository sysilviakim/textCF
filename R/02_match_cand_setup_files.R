source(here::here("R", "hcan_setup_2020.R"))
source(here::here("R", "scan_setup_2020.R"))
# source(here::here("R", "hcan_setup_2022.R"))
# source(here::here("R", "scan_setup_2022.R"))

# df_list <- c("actblue", "winred", "anedot") %>%
#   set_names(., .) %>%
#   map(~ loadRData(here("data", "tidy", paste0(.x, "_federal_2022.Rda"))))

actblue <- loadRData(here("data", "tidy", "actblue_federal_2020.Rda")) %>%
  filter(
    ## Switched from House candidate to Senate in 2022
    ## Text data gathered Feb 3, 2021, so already switched to Senate
    display_name != "Allen Ellison" & 
      ## Similarly switched to Senate
      display_name != "Alan Grayson" & 
      ## https://www.fec.gov/data/candidate/S0LA00402/
      ## Never set up because not displayed in the race; withdrew primaries
      display_name != "Drew Knight" & 
      ## Senate 2022 candidate
      display_name != "Tom Nelson" &
      ## President
      display_name != "Cecilia Okugo" & 
      ## House 2022 candidates
      display_name != "Brian Jaye" &
      display_name != "Aarika Rhodes" &
      ## Minor candidates
      display_name != "Brody Pierrottie" &
      display_name != "Antonio Cortez Hicks" &
      display_name != "Matt Boswell" &
      display_name != "Jason Kishineff" & 
      display_name != "Mary Helen Barro" &
      display_name != "Regina Marston" & 
      display_name != "Tony Le" & 
      display_name != "Valbrun Almonord" & 
      ## Only 50 states
      display_pretty_location != "MP-AL" & 
      display_pretty_location != "VI-AL" & 
      display_pretty_location != "GU-AL" & 
      display_pretty_location != "PR-AL" & 
      display_pretty_location != "AS-AL"
  ) %>%
  mutate(display_name = gsub(" 2020$|, Jr$| Jr$", "", display_name))

# Match ActBlue to setup file ==================================================
df <- bind_rows(
  cand_senate_2020 %>% mutate(chamber = "senate"),
  cand_house_2020 %>% mutate(chamber = "house")
  # cand_senate_2022 %>% mutate(chamber = "senate"),
  # cand_house_2022 %>% mutate(chamber = "house")
) %>%
  mutate(
    display_name = paste(first_name, last_name),
    display_pretty_location = case_when(
      chamber == "house" ~ gsub("-0$", "-AL", state_cd),
      chamber == "senate" ~ paste0(state, "-Sen")
    )
  ) %>%
  select(display_name, display_pretty_location, chamber, year, everything()) %>%
  select(
    -actblue_link, -anedot_link, -other_link,
    -FEC_ID_cmte, -campaign_name, -campaign_website
  ) %>%
  dedup() %>%
  arrange(chamber, display_pretty_location, display_name, year) %>%
  filter(party != "Rep" & party != "Lib") %>%
  mutate(
    display_name = case_when(
      display_name == "Edward Markey" ~ "Ed Markey",
      display_name == "Christopher Coons" ~ "Chris Coons",
      display_name == "Al Gross" ~ "Alan Gross",
      display_name == "Liz Hernandez" ~ "Elizabeth Hernandez",
      display_name == "Jim McGovern" ~ "James McGovern",
      display_name == "Erika Stotts-Pearson" ~ "Erika Stotts Pearson",
      TRUE ~ display_name
    )
  )
  ## Duplicate IDs for Audra Lemons-Johnson
  # filter(year == 2022 & FEC_ID_cand != "H2MI03171")

assert_that(!any(is.na(df$display_name)))
assert_that(!any(is.na(df$display_pretty_location)))
assert_that(
  df %>%
    group_by(display_name, display_pretty_location, year) %>%
    filter(n() > 1) %>%
    nrow() == 0
)
intersect(names(df), names(actblue))
assert_that(
  anti_join(actblue, df) %>%
    select(display_name, display_pretty_location) %>%
    dedup() %>%
    nrow() == 0
)

## Incumbents who were defeated taken out by Feb :(
## Must manually add
anti_join(df, actblue)
