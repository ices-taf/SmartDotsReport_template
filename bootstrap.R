## Preprocess data, write TAF data tables

## Before:
## After: report_template.docx, dist.csv,
##        ad_long.csv, ad_long_ex.csv,
##        ad_wide.csv, ad_wide_ex.csv

library(icesTAF)
library(jsonlite)

# create data directory
mkdir("bootstrap")

# load configuration
config <- read_json("config.json", simplifyVector = TRUE)

# get data from api --------
msg("downloading annotations for event id ... ", config$event_id)
ad <-
  read.csv(
    paste0("http://localhost:1234/api/smartDots/Annotations/", config$event_id),
    stringsAsFactors = FALSE)

msg("downloading dots for event id... ", config$event_id)
dist <-
  read.csv(
    paste0("http://localhost:1234/api/smartDots/DotsDistances/", config$event_id),
    stringsAsFactors = FALSE)


# write out 'bootstrap' data tables
write.taf(dist, "bootstrap/dist.csv")
write.taf(ad, "bootstrap/data.csv")
