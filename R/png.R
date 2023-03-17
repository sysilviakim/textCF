png(
  here("fig", "anger_by_type_chamber.png"),
  width = 6 * 300, height = 2.8 * 300, res = 300
)
print(
  fb_mention_plot(
    emotion_summ %>% rename(Party = type),
    xvar = "mean_anger", se = "se_anger", xlab = ""
  ) +
    scale_x_continuous(limits = c(0, 0.09), labels = scales::percent)
)
dev.off()

png(
  here("fig", "disgust_by_type_chamber.png"),
  width = 6 * 300, height = 2.8 * 300, res = 300
)
print(
  fb_mention_plot(
    emotion_summ %>% rename(Party = type),
    xvar = "mean_disgust", se = "se_disgust", xlab = ""
  ) +
    scale_x_continuous(limits = c(0, 0.09), labels = scales::percent)
)
dev.off()

png(
  here("fig", "fear_by_type_chamber.png"),
  width = 6 * 300, height = 2.8 * 300, res = 300
)
print(
  fb_mention_plot(
    emotion_summ %>% rename(Party = type),
    xvar = "mean_fear", se = "se_fear", xlab = ""
  ) +
    scale_x_continuous(limits = c(0, 0.09), labels = scales::percent)
)
dev.off()

png(
  here("fig", "wordcloud_dem_donor.png"),
  width = 4 * 300, height = 2.5 * 300, res = 300
)
textplot_wordcloud(
  fb_dfm$dem_donor,
  max_words = 100, rotation = .25,
  color = RColorBrewer::brewer.pal(9, "Blues")[4:9]
)
dev.off()

png(
  here("fig", "wordcloud_dem_voter.png"),
  width = 4 * 300, height = 2.5 * 300, res = 300
)
textplot_wordcloud(
  fb_dfm$dem_voter,
  max_words = 100, rotation = .25,
  color = RColorBrewer::brewer.pal(9, "Blues")[4:9]
)
dev.off()

png(
  here("fig", "wordcloud_rep_donor.png"),
  width = 4 * 300, height = 2.5 * 300, res = 300
)
textplot_wordcloud(
  fb_dfm$rep_donor,
  max_words = 100, rotation = .2, ## I want "help" to be not rotated
  color = RColorBrewer::brewer.pal(9, "Blues")[4:9]
)
dev.off()

png(
  here("fig", "wordcloud_rep_voter.png"),
  width = 4 * 300, height = 2.5 * 300, res = 300
)
textplot_wordcloud(
  fb_dfm$rep_voter,
  max_words = 100, rotation = .25,
  color = RColorBrewer::brewer.pal(9, "Blues")[4:9]
)
dev.off()

png(
  here("fig", "in_district_per_type_chamber.png"),
  width = 6 * 300, height = 2.8 * 300, res = 300
)
print(
  fb_mention_plot(temp, xvar = "mean", se = "se", xlab = "") +
    scale_x_continuous(limits = c(0, 1))
)
dev.off()

etable(
  fit_se_cluster,
  cluster = "Candidate", replace = TRUE, dict = varlabel,
  drop = c("Constant", "state_po")
)

etable(
  fit_fe,
  replace = TRUE,
  dict = varlabel, drop = c("Constant", "state_po")
)

png(
  here("fig", "fb_toxic_means_by_type_chamber.png"),
  width = 6 * 300, height = 2.8 * 300, res = 300
)
print(p1)
dev.off()

png(
  here("fig", "fb_toxic_means_by_type_chamber_all.png"),
  width = 6 * 300, height = 2.8 * 300, res = 300
)
print(p2)
dev.off()

png(
  here("fig", "n_unique_ads_over_time.png"),
  width = 5 * 300, height = 3.5 * 300, res = 300
)
print(
  plot_notitle(png_default(p)) + theme(legend.position = "bottom") +
    guides(colour = guide_legend(nrow = 2))
)
dev.off()

png(
  here("fig", "toxicity_by_party_trump_chamber.png"),
  width = 6 * 300, height = 4 * 300, res = 300
)
print(plot_nolegend(png_default(p)))
dev.off()

png(
  here("fig", "mention_trump_by_type_chamber.png"), 
  width = 6 * 300, height = 2.8 * 300, res = 300
)
print(
  fb_mention_plot(
    trump_df,
    xvar = "mean_trump", se = "se_trump", 
    xlab = "Mentions Trump"
  )
)
dev.off()
