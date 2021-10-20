source(here::here("R", "utilities.R"))

## For consistency, use *all* available WayBack snapshots for *all* candidates
cong <- list(
  senate = loadRData(here("data", "tidy", "senate-2020-matched.Rda")),
  house = loadRData(here("data", "tidy", "house-2020-matched.Rda"))
)

# Check WayBack Machine mementos: donation pages download ======================
for (x in c("house", "senate")) {
  ## Define file path
  fp <- c("data", "raw", "wayback", x, "timestamp_donation") %>%
    paste(collapse = "/")
  if (!dir.exists(fp)) dir.create(fp, recursive = TRUE)
  
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