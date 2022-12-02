## Preprocess data, write TAF data tables

## Before:
## After: report_template.docx, dist.csv,
##        ad_long.csv, ad_long_ex.csv,
##        ad_wide.csv, ad_wide_ex.csv

library(icesTAF)
library(jsonlite)

# load configuration
config <- read_json(taf.data.path("config.json"), simplifyVector = TRUE)

# get data from api --------
zipfile <- "smartdots_data.zip"
download.file(
  paste0("https://smartdots.ices.dk/download/DownloadEvent.ashx?Token=3F482827-282B-46E0-8C58-FBCF6C4B2C55&tblEventID=", config$event_id),
  zipfile,
  mode = "wb"
)

files <- unzip(zipfile, list = TRUE)$Name
files <- files[grep("*.csv", files)]
unzip(zipfile, files = files, exdir = ".")

dist <- read.csv(files[grep("DotsDistances",  files)], stringsAsFactors = FALSE)
data <- read.csv(files[grep("Annotations",  files)], stringsAsFactors = FALSE)

# delete downloaded data
unlink(files)
unlink(zipfile)

# drop comments feild
data <- data[, names(data) != "Comment"]

# write out 'bootstrap' data tables
write.taf(dist, quote = TRUE)
write.taf(data, quote = TRUE)
