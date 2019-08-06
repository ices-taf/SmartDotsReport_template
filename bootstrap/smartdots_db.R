## Preprocess data, write TAF data tables

## Before:
## After: report_template.docx, dist.csv,
##        ad_long.csv, ad_long_ex.csv,
##        ad_wide.csv, ad_wide_ex.csv

library(icesTAF)
library(jsonlite)


# load configuration
config <- read_json("../config.json", simplifyVector = TRUE)

# get data from api --------
zipfile <- "smartdots_data.zip"
download.file(paste0("https://smartdots.ices.dk/download/DownloadEvent.ashx?tblEventID=", config$event_id),
              zipfile,  mode = "wb")
files <- unzip(zipfile, list = TRUE)$Name
files <- files[grep("*.csv", files)]
unzip(zipfile, files = files, exdir = ".")

# read in and write out again
dist <- read.csv(files[grep("DotsDistances",  files)], stringsAsFactors = FALSE)
ad <- read.csv(files[grep("Annotations",  files)], stringsAsFactors = FALSE)

# delete downloaded data
unlink(files)

# drop comments feild
ad <- ad[,names(ad) != "Comment"]

# write out 'bootstrap' data tables
write.taf(dist, "dist.csv", quote = TRUE)
write.taf(ad, "data.csv", quote = TRUE)
