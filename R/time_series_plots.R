source(here::here("R", "utilities.R"))

# Part 1: Unique Ads over time =================================================
## fb_unique doesn't contain dates, so we'll need to go to to fb_matched
load(here("data/tidy/fb_matched.Rda"))
matched_house <- fb_matched[["house"]]
matched_senate <- fb_matched[["senate"]]
unique_house <- matched_house[!duplicated(matched_house[ , c("ad_creative_body")]), ]
unique_senate <- matched_senate[!duplicated(matched_senate[ , c("ad_creative_body")]), ]

hr_unique_ts <- ggplot(unique_house, aes(x=ad_creation_time)) +  
  theme_bw() +  geom_histogram(binwidth=14, fill="darkblue",color="black") +
  labs(x="Date", y="Number of Unique Advertisements Created") + 
  ggtitle("House") +
  theme(axis.text.x=element_text(angle=30, hjust=1),
        plot.title = element_text(hjust = 0.5)) +
  scale_x_date(date_breaks = "months", date_labels = "%Y-%m-%d") +#, 
             #  limits = as.Date(c("2019-01-01", "2020-12-31"))) +
  lims(x = as.Date(c("2019-01-01", "2020-12-31")))
hr_unique_ts

sen_unique_ts <- ggplot(unique_senate, aes(x=ad_creation_time)) +  
  theme_bw() +  
  geom_histogram(binwidth=14, fill="darkblue",color="black") +
  labs(x="Date", y="Number of Unique Advertisements Created") + 
  ggtitle("Senate") +
  theme(axis.text.x=element_text(angle=30, hjust=1),
        plot.title = element_text(hjust = 0.5)) +
  scale_x_date(date_breaks = "months", date_labels = "%Y-%m-%d") +#, 
  #  limits = as.Date(c("2019-01-01", "2020-12-31"))) +
  lims(x = as.Date(c("2019-01-01", "2020-12-31")))
sen_unique_ts

unique_ts <- ggarrange(hr_unique_ts, sen_unique_ts, ncol = 2, nrow = 1)
unique_ts <- annotate_figure(unique_ts, top = text_grob("Unique Ads Created Over Time", 
                                      color = "black", face = "bold", 
                                      size = 14))
unique_ts

ggsave("fig/unique_ts.pdf", plot = unique_ts, width = 8, height = 6)

# Part 2: Impressions over time ================================================

## THIS SHOULD NOT BE UNIQUE! This should be the full sets of ads from over in
## fb_matched.

impdat_house <- select(fb_matched[["house"]], 
                       c("ad_creation_time", "impressions_lower"))
names(impdat_house) <- c("date", "impressions")
aggregate_house <- aggregate(.~date,data=impdat_house,FUN=sum)

hr_imp_ts <- ggplot(aggregate_house, aes(date, impressions)) +
  theme_bw() +
  geom_bar(stat="identity", na.rm = TRUE) +
  ggtitle("House") +
  xlab("Ad Creation Date") + ylab("Number of Impressions") +
  #scale_x_date(labels=date_format ("%b %y"), breaks=date_breaks("1 year")) +
  theme(axis.text.x=element_text(angle=30, hjust=1),
        plot.title = element_text(hjust = 0.5)) +
  scale_x_date(date_breaks = "months", date_labels = "%Y-%m-%d") +#, 
  #  limits = as.Date(c("2019-01-01", "2020-12-31"))) +
  lims(x = as.Date(c("2019-01-01", "2020-12-31")))
hr_imp_ts

impdat_sen <- select(fb_matched[["senate"]], 
                     c("ad_creation_time", "impressions_lower"))
names(impdat_sen) <- c("date", "impressions")
aggregate_sen <- aggregate(.~date,data=impdat_sen,FUN=sum)

s_imp_ts <- ggplot(aggregate_sen, aes(date, impressions)) +
  theme_bw() +
  geom_bar(stat="identity", na.rm = TRUE) +
  ggtitle("Senate") +
  xlab("Ad Creation Date") + ylab("Number of Impressions") +
  #scale_x_date(labels=date_format ("%b %y"), breaks=date_breaks("1 year")) +
  theme(axis.text.x=element_text(angle=30, hjust=1),
        plot.title = element_text(hjust = 0.5)) +
  scale_x_date(date_breaks = "months", date_labels = "%Y-%m-%d") +#, 
  #  limits = as.Date(c("2019-01-01", "2020-12-31"))) +
  lims(x = as.Date(c("2019-01-01", "2020-12-31")))
s_imp_ts

impressions_ts <- ggarrange(hr_imp_ts, s_imp_ts, ncol = 2, nrow = 1)
impressions_ts <- annotate_figure(impressions_ts, 
                                  top = text_grob("Impressions Over Time (Lower Estimate)", 
                                                        color = "black", 
                                                  face = "bold", 
                                                        size = 14))
impressions_ts
ggsave("fig/impressions_ts.pdf", plot = impressions_ts, width = 8, height = 6)

# Part 3: Amounts Spent Over Time ==============================================

## THIS SHOULD NOT BE UNIQUE! This should be the full sets of ads from over in
## fb_matched.

funddat_house <- select(fb_matched[["house"]], 
                       c("ad_creation_time", "spend_lower"))
names(funddat_house) <- c("date", "funds")
ag_fund_house <- aggregate(.~date,data=funddat_house,FUN=sum)

hr_fund_ts <- ggplot(ag_fund_house, aes(date, funds)) +
  theme_bw() +
  geom_bar(stat="identity", na.rm = TRUE) +
  ggtitle("House") +
  xlab("Ad Creation Date") + ylab("Total Funds Spent") +
  #scale_x_date(labels=date_format ("%b %y"), breaks=date_breaks("1 year")) +
  theme(axis.text.x=element_text(angle=30, hjust=1),
        plot.title = element_text(hjust = 0.5)) +
  scale_x_date(date_breaks = "months", date_labels = "%Y-%m-%d") +#, 
  #  limits = as.Date(c("2019-01-01", "2020-12-31"))) +
  lims(x = as.Date(c("2019-01-01", "2020-12-31")))
hr_fund_ts

funddat_sen <- select(fb_matched[["senate"]], 
                     c("ad_creation_time", "spend_lower"))
names(funddat_sen) <- c("date", "funds")
ag_fund_sen <- aggregate(.~date,data=funddat_sen,FUN=sum)

s_fund_ts <- ggplot(ag_fund_sen, aes(date, funds)) +
  theme_bw() +
  geom_bar(stat="identity", na.rm = TRUE) +
  ggtitle("Senate") +
  xlab("Ad Creation Date") + ylab("Total Funds Spent") +
  #scale_x_date(labels=date_format ("%b %y"), breaks=date_breaks("1 year")) +
  theme(axis.text.x=element_text(angle=30, hjust=1),
        plot.title = element_text(hjust = 0.5)) +
  scale_x_date(date_breaks = "months", date_labels = "%Y-%m-%d") +#, 
  #  limits = as.Date(c("2019-01-01", "2020-12-31"))) +
  lims(x = as.Date(c("2019-01-01", "2020-12-31")))
s_fund_ts

funds_ts <- ggarrange(hr_fund_ts, s_fund_ts, ncol = 2, nrow = 1)
funds_ts <- annotate_figure(funds_ts, 
                                  top = text_grob("Total Funds Spent on Ad Creation (Lower Estimate)", 
                                                  color = "black", 
                                                  face = "bold", 
                                                  size = 14))
funds_ts
ggsave("fig/funds_ts.pdf", plot = funds_ts, width = 8, height = 6)

# Part 4: Financial/Non-Financial Time Series ==================================
# Now, a slight hiccup for the next ones...fb_unique does not have the dates.
# So, we shall recreate a version of fb_unique that features the dates the ads
# were created. Will pull other variables of interest as well -- can potentially
# shift some things around, include more variables, later on if we wish

# Deduplicated, simplified ad data
fb_unique_dates <- fb_matched %>%
  map(
    ~ .x %>%
      select(
        candidate,
        fb_ad_library_id, page_name, party, inc, state_po, contains("state_cd"),
        ad_creative_body, ad_creative_link_caption, vote_share, ad_creation_time
      ) %>%
      distinct() %>%
      filter(ad_creative_body != "") %>%
      ## Antonio Delgado duplicates appear (House, with wrong PID)
      ## Drop these rows
      filter(!(page_name == "Antonio Delgado" & party == "REPUBLICAN")) %>%
      ## Label ad types by conduit/platform
      mutate(ad_creative_link_caption = tolower(ad_creative_link_caption)) %>%
      rowwise() %>%
      ## Perform twice to unify to similar patterns
      simplify_ad_body() %>%
      simplify_ad_body() %>%
      ## Classify [ongoing!]
      mutate(
        ## Is the typical platform links in the caption link?
        type = case_when(
          grepl("actblue.com", ad_creative_link_caption) ~ "ActBlue",
          grepl("winred.com", ad_creative_link_caption) ~ "WinRed",
          grepl("ngpvan.com|myngp.com", ad_creative_link_caption) ~ "NGP VAN",
          grepl("anedot.com", ad_creative_link_caption) ~ "Anedot",
          grepl("victorypassport.com", ad_creative_link_caption) ~
            "Victory Passport",
          grepl("fundraiser", ad_creative_link_caption) ~ "Misc.",
          is.na(ad_creative_link_caption) ~ "Non-financial",
          # grepl(" ", ad_creative_link_caption) ~ "Non-financial",
          grepl(
            "conversation with |town hall|meet |tour stop |iwillvote.com",
            ad_creative_link_caption
          ) ~ "Non-financial",
          grepl(".gov", ad_creative_link_caption) ~ "Government Information",
          grepl("secure.|act.|action.|go.", ad_creative_link_caption) ~
            "Personal Contribution Link",
          grepl("facebook.com|fb.me", ad_creative_link_caption) ~
            "Facebook Page",
        )
      ) %>%
      donate_classify() %>%
      ungroup()
  )

## Part A: Financial/Non-Financial in the House
house_unique <- fb_unique_dates[["house"]]
house_financial <- house_unique[house_unique$financial == "Financial", ] 
house_nonfinancial <- house_unique[house_unique$financial == "Non-Financial", ]

hr_financial <- ggplot(house_financial, aes(x=ad_creation_time)) +  
  theme_bw() +  geom_histogram(binwidth=14, fill="darkblue",color="black") +
  labs(x="Date", y="Number of Unique Advertisements Created") + 
  ggtitle("Financial") +
  theme(axis.text.x=element_text(angle=30, hjust=1),
        plot.title = element_text(hjust = 0.5)) +
  scale_x_date(date_breaks = "months", date_labels = "%Y-%m-%d") +#, 
  #  limits = as.Date(c("2019-01-01", "2020-12-31"))) +
  lims(x = as.Date(c("2019-01-01", "2020-12-31")))
hr_financial

hr_nonfinancial <- ggplot(house_nonfinancial, aes(x=ad_creation_time)) +  
  theme_bw() +  geom_histogram(binwidth=14, fill="darkblue",color="black") +
  labs(x="Date", y="Number of Unique Advertisements Created") + 
  ggtitle("Non-Financial") +
  theme(axis.text.x=element_text(angle=30, hjust=1),
        plot.title = element_text(hjust = 0.5)) +
  scale_x_date(date_breaks = "months", date_labels = "%Y-%m-%d") +#, 
  #  limits = as.Date(c("2019-01-01", "2020-12-31"))) +
  lims(x = as.Date(c("2019-01-01", "2020-12-31")))
hr_nonfinancial

house_financial <- ggarrange(hr_financial, hr_nonfinancial, ncol = 2, nrow = 1)
house_financial <- annotate_figure(house_financial, 
                            top = text_grob("Unique House Advertisements by Creation Date", 
                                            color = "black", 
                                            face = "bold", 
                                            size = 14))
house_financial
ggsave("fig/house_financial.pdf", plot = house_financial, width = 8, height = 6)


## Part B: Financial/Non-Financial in the Senate
senate_unique <- fb_unique_dates[["senate"]]
senate_financial <- senate_unique[senate_unique$financial == "Financial", ] 
senate_nonfinancial <- senate_unique[senate_unique$financial == "Non-Financial", ]

sen_financial <- ggplot(senate_financial, aes(x=ad_creation_time)) +  
  theme_bw() +  geom_histogram(binwidth=14, fill="darkblue",color="black") +
  labs(x="Date", y="Number of Unique Advertisements Created") + 
  ggtitle("Financial") +
  theme(axis.text.x=element_text(angle=30, hjust=1),
        plot.title = element_text(hjust = 0.5)) +
  scale_x_date(date_breaks = "months", date_labels = "%Y-%m-%d") +#, 
  #  limits = as.Date(c("2019-01-01", "2020-12-31"))) +
  lims(x = as.Date(c("2019-01-01", "2020-12-31")))
sen_financial

sen_nonfinancial <- ggplot(senate_nonfinancial, aes(x=ad_creation_time)) +  
  theme_bw() +  geom_histogram(binwidth=14, fill="darkblue",color="black") +
  labs(x="Date", y="Number of Unique Advertisements Created") + 
  ggtitle("Non-Financial") +
  theme(axis.text.x=element_text(angle=30, hjust=1),
        plot.title = element_text(hjust = 0.5)) +
  scale_x_date(date_breaks = "months", date_labels = "%Y-%m-%d") +#, 
  #  limits = as.Date(c("2019-01-01", "2020-12-31"))) +
  lims(x = as.Date(c("2019-01-01", "2020-12-31")))
sen_nonfinancial

senate_financial <- ggarrange(sen_financial, sen_nonfinancial, 
                             ncol = 2, nrow = 1)
senate_financial <- annotate_figure(senate_financial, 
                                   top = text_grob("Unique Senate Advertisements by Creation Date", 
                                                   color = "black", 
                                                   face = "bold", 
                                                   size = 14))
senate_financial
ggsave("fig/senate_financial.pdf", 
       plot = senate_financial, width = 8, height = 6)

## After this point, moved figures to fig/time_series_plots folder

# Part 5: Toxicity Time Series? ================================================

## Is this one doable?

#mergetest1 <- merge(house_unique, fb_perspective_House, 
#                    by.x = "ad_creative_body", by.y = "ad_creative_body")
#mergetest2 <- merge(fb_perspective_House, house_unique,
#                    by.x = "ad_creative_body", by.y = "ad_creative_body")
# Both of these aggressively failed. Perhaps a different approach will work? 
mergetest3 <- merge(x = fb_perspective_House, 
                    y = house_unique[ , c("ad_creative_body", "ad_creation_time")], 
                    by = "ad_creative_body", all.x=TRUE)
toxic_house <- mergetest3 %>% distinct(ad_creative_body, .keep_all = TRUE)

senatemerge <- merge(x = fb_perspective_Senate, 
                    y = senate_unique[ , c("ad_creative_body", "ad_creation_time")], 
                    by = "ad_creative_body", all.x=TRUE)
toxic_senate <- senatemerge %>% distinct(ad_creative_body, .keep_all = TRUE)

toxdat_house <- select(toxic_house,
                       c("ad_creation_time", "TOXICITY"))
names(toxdat_house) <- c("date", "toxic")
ag_tox_house <- aggregate(.~date,data=toxdat_house,FUN=mean)

hr_toxic_ts <- ggplot(ag_tox_house, aes(date, toxic)) +
  theme_bw() +
  geom_bar(stat="identity", na.rm = TRUE) +
  ggtitle("House") +
  xlab("Ad Creation Date") + ylab("Average Toxicity") +
  #scale_x_date(labels=date_format ("%b %y"), breaks=date_breaks("1 year")) +
  theme(axis.text.x=element_text(angle=30, hjust=1),
        plot.title = element_text(hjust = 0.5)) +
  scale_x_date(date_breaks = "months", date_labels = "%Y-%m-%d") +#, 
  #  limits = as.Date(c("2019-01-01", "2020-12-31"))) +
  lims(x = as.Date(c("2019-01-01", "2020-12-31")))
hr_toxic_ts

toxdat_sen <- select(toxic_senate,
                       c("ad_creation_time", "TOXICITY"))
names(toxdat_sen) <- c("date", "toxic")
ag_tox_sen <- aggregate(.~date,data=toxdat_sen,FUN=mean)

sen_toxic_ts <- ggplot(ag_tox_sen, aes(date, toxic)) +
  theme_bw() +
  geom_bar(stat="identity", na.rm = TRUE) +
  ggtitle("Senate") +
  xlab("Ad Creation Date") + ylab("Average Toxicity") +
  #scale_x_date(labels=date_format ("%b %y"), breaks=date_breaks("1 year")) +
  theme(axis.text.x=element_text(angle=30, hjust=1),
        plot.title = element_text(hjust = 0.5)) +
  scale_x_date(date_breaks = "months", date_labels = "%Y-%m-%d") +#, 
  #  limits = as.Date(c("2019-01-01", "2020-12-31"))) +
  lims(x = as.Date(c("2019-01-01", "2020-12-31")))
sen_toxic_ts

toxic_ts <- ggarrange(hr_toxic_ts, sen_toxic_ts, ncol = 2, nrow = 1)
toxic_ts <- annotate_figure(toxic_ts, 
                            top = text_grob("Average Advertisement Toxicity by Creation Date", 
                                            color = "black", 
                                            face = "bold", 
                                            size = 14))
toxic_ts
ggsave("fig/time_series_figures/toxic_ts.pdf", plot = toxic_ts, 
       width = 8, height = 6)

