source(here::here("R", "utilities.R"))

## For consistency, use *all* available WayBack snapshots for *all* candidates
cong <- list(
  senate = loadRData(here("data", "tidy", "senate-2020-matched.Rda")) %>%
    mutate(state_cd = paste0(state, "-0")),
  house = loadRData(here("data", "tidy", "house-2020-matched.Rda"))
)

# Check WayBack Machine mementos: donation pages download ======================
for (x in c("house", "senate")) {
  ## Define file path
  fp <- c("data", "raw", "wayback", x, "timestamp_donation") %>%
    paste(collapse = "/")
  if (!dir.exists(fp)) {
    dir.create(fp, recursive = TRUE)
    dir.create(gsub("timestamp_donation", "html", fp), recursive = TRUE)
  }
  
  ## Loop
  for (i in seq(nrow(cong[[x]]))) {
    tryCatch(
      {
        download.file(
          wayback_timemap(cong[[x]], i, var = "url"),
          destfile = here(
            fp,
            paste0(
              "wayback_timemap_", 
              file_pattern(cong[[x]], i, var = "url"), ".txt"
            )
          )
        )
      },
      error = function(e) message(e)
    )
    Sys.sleep(5)
  }
}

# Are there any failed URLs? ===================================================
# It happens! e.g., https://johncastro.com
# "This URL has been excluded from the Wayback Machine."
for (x in c("house", "senate")) {
  print(
    wayback_timemap_exceptions(
      cong[[x]], c("data", "raw", "wayback", x, "timestamp_donation") %>%
        paste(collapse = "/"), var = "url"
    )
  )
}

# Check WayBack Machine mementos: donation page (read/daily wrangle) ===========
for (x in c("house", "senate")) {
  fp <- c("data", "raw", "wayback", x, "timestamp_donation") %>%
    paste(collapse = "/")
  
  ## Read mementos into a single dataframe
  wayback_donate <- 
    list.files(here(fp), pattern = ".txt", full.names = TRUE) %>%
    wayback_memento()
  assert_that(!any(is.na(wayback_donate$dt)))
  assert_that(length(unique(wayback_donate$dt)) > 1)
  save(
    wayback_donate, 
    file = here(fp, paste0(x, "_donation_wayback_timestamps.Rda"))
  )
  
  ## Merge candidate data with WayBack timestamps, 2019+
  merged_df <- wayback_merge(wayback_donate, cong[[x]],  var = "url") %>%
    arrange(state, last_name, first_name, date) %>%
    dedup()
  assert_that(sum(is.na(merged_df$year)) == 0)
  
  ## Save
  fp_out <- here("data", "tidy", "wayback", x)
  if (!dir.exists(fp_out)) dir.create(fp_out, recursive = TRUE)
  
  save(
    merged_df,
    file = here(
      "data", "tidy", "wayback", x, "wayback_senate_merged_donation.Rda"
    )
  )
  
  ## Check/download WayBack Machine snapshots: donation page daily, source
  for (i in seq(nrow(merged_df))) {
    url <- merged_df$link[i]
    con <- tryCatch(read_html(url), error = function(e) print(e))
    
    if (sum(class(con) == "error") < 1) {
      write_xml(con, file = wayback_stamp_html(merged_df, i, fp))
    }
    message(wayback_msg(merged_df, i))
    Sys.sleep(5)
  }
}
