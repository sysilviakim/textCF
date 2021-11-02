library(tidyverse)
library(haven)

# SK, please don't throw stones at me for loading locally stored dta files!

C2008 <- read_dta("~/Dropbox/B_polls/CCES2008/cces_2008_common.dta") %>%
  transmute(year = 2008,
            donated = ifelse(CC415_6==1,1,0),
         democrat = ifelse(CC424 <= 3,1,0),
         republican = ifelse(CC424 == 5 | CC424 == 6 | CC424 == 7,1,0),
         midterm_year = 0,
         w = V201)


C2010 <- read_dta("~/Dropbox/B_polls/CCES2010/cces_2010_common_validated.dta") %>%
  transmute(year = 2010,
            donated = ifelse(CC417a_4==1,1,0),
            democrat = ifelse(V212d <= 3,1,0),
            republican = ifelse(V212d == 5 | V212d == 6 | V212d == 7,1,0),
            midterm_year = 1,
            w = V101)

C2016 <- read_dta("~/Dropbox/B_polls/CCES2016/CCES16_Common_OUTPUT_Feb2018_VV.dta") %>%
  transmute(year = 2016,
            donated = ifelse(CC16_417a_4==1,1,0),
            democrat = ifelse(pid7 <= 3,1,0),
            republican = ifelse(pid7 == 5 | pid7 == 6 | pid7 == 7,1,0),
            midterm_year = 10,
            w = commonweight,
            wp = commonweight_post)

C2018 <- read_dta("~/Dropbox/B_polls/CCES2018_CC/cces18_common_vv.dta") %>%
  transmute(year = 2018,
         donated = ifelse(CC18_417a_6==1,1,0),
         democrat = ifelse(pid7 <= 3,1,0),
         republican = ifelse(pid7 == 5 | pid7 == 6 | pid7 == 7,1,0),
         midterm_year = 1,
         w = commonweight,
         wp = commonpostweight)

C2020 <- read_dta("~/Dropbox/B_polls/CES2020/CES20_Common_OUTPUT_vv.dta") %>%
  transmute(year = 2020,
            donated = ifelse(CC20_430a_6==1,1,0),
            protested = ifelse(CC20_430a_4==1,1,0),
            democrat = ifelse(pid7 <= 3,1,0),
            republican = ifelse(pid7 == 5 | pid7 == 6 | pid7 == 7,1,0),
            midterm_year = 0,
            w = commonweight,
            wp = commonpostweight)


CC <- bind_rows(C2008,
                C2010,
                C2016,
                C2018,
                C2020) %>%
  mutate(PID = case_when(democrat == 1 ~ "Democrats",
                         republican == 1 ~ "Republicans"))



library(pollster)
pollster::crosstab(df = CC,
         x = year,
         y = donated,
         weight = w,
         unwt_n = TRUE)

pollster::crosstab_3way(df = CC,
                        x = year,
                        y = donated,
                        z = PID,
                        weight = w,
                        unwt_n = TRUE) %>%
  rename(`Did not donate` = `0`,
         `Donated` = `1`,
         `Weighted N` = n) %>%
  arrange(PID)

pollster::crosstab_3way(df = CC %>% filter(midterm_year==1),
                   x = year,
                   y = donated,
                   z = PID,
                   weight = w,
                   unwt_n = TRUE) %>%
  rename(`Did not donate` = `0`,
         `Donated` = `1`,
         `Weighted N` = n)
