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
ad <-
  read.csv(
    paste0("http://localhost:1234/api/smartDots/Annotations/", config$event_id),
    stringsAsFactors = FALSE)

dist <-
  read.csv(
    paste0("http://localhost:1234/api/smartDots/DotsDistances/", config$event_id),
    stringsAsFactors = FALSE)


# write out 'begin' data tables
write.taf(dist, "begin/dist.csv")
write.taf(ad, "begin/data.csv")
