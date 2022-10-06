## Preprocess data, write TAF data tables

## Before:
## After: report_template.docx, dist.csv,
##        ad_long.csv, ad_long_ex.csv,
##        ad_wide.csv, ad_wide_ex.csv

library(icesTAF)
library(jsonlite)

# # load configuration
config <- read_json(taf.data.path("config.json"), simplifyVector = TRUE)

# Download the reader´s expertise data in case the event organizer choose "multistage" approach to estimate the modal age. Download the data,  create and save a csv file.
if (config$mode_definition == "multistage") {
  # download the information regarding the expertise of the readers

  expurl <-
    paste0(
      "https://smartdots.ices.dk/api/Reporting/getReportingVariablesRandom?",
      "tblEventID=", config$event_id
    )

  expdat <- fromJSON(expurl)

  # bug fix
  msg("WARNING: Manually creating stockCode and iceS_Area feilds! and setting to NA")
  expdat$stockCode <- NA
  expdat$iceS_Area <- NA

  write.taf(expdat, "expdat.csv", quote = TRUE)
}

# get data from api --------
zipfile <- "smartdots_data.zip"

url <-
  paste0(
    "https://smartdots.ices.dk/download/DownloadEvent.ashx?",
    "Token=", config$token,
    "&tblEventID=", config$event_id
  )

download.file(url,
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
