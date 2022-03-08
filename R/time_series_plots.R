source(here::here("R", "utilities.R"))

# Part 1: Unique Ads over time
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

# Part 2: Impressions over time

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

# Part 3: Amounts Spent Over Time

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

