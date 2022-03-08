source(here::here("R", "utilities.R"))

# Part 1: Unique Ads over time
## fb_unique doesn't contain dates, so we'll need to go to to fb_matched
load(here("data/tidy/fb_matched.Rda"))
matched_house <- fb_matched[["house"]]
matched_senate <- fb_matched[["senate"]]
unique_house <- matched_house[!duplicated(matched_house[ , c("ad_creative_body")]), ]  # Delete rows
unique_senate <- matched_senate[!duplicated(matched_senate[ , c("ad_creative_body")]), ]  # Delete rows

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
  theme_bw() +  geom_histogram(binwidth=14, fill="darkblue",color="black") +
  labs(x="Date", y="Number of Unique Advertisements Created") + 
  ggtitle("Senate") +
  theme(axis.text.x=element_text(angle=30, hjust=1),
        plot.title = element_text(hjust = 0.5)) +
  scale_x_date(date_breaks = "months", date_labels = "%Y-%m-%d") +#, 
  #  limits = as.Date(c("2019-01-01", "2020-12-31"))) +
  lims(x = as.Date(c("2019-01-01", "2020-12-31")))
sen_unique_ts

unique_ts <- ggarrange(hr_unique_ts, sen_unique_ts, ncol = 2, nrow = 1) +
  ggtitle("Number of Unique Advertisements Created Over Time")

