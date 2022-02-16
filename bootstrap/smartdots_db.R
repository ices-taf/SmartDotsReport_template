## Preprocess data, write TAF data tables

## Before:
## After: report_template.docx, dist.csv,
##        ad_long.csv, ad_long_ex.csv,
##        ad_wide.csv, ad_wide_ex.csv

library(icesTAF)
library(jsonlite)

# # load configuration
config <- read_json("initial/data/config.json", simplifyVector = TRUE)

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


# Download the reader´s expertise data in case the event organizer choose "multistage" approach to estimate the modal age. Download the data,  create and save a csv file.
if(config$mode_definition=="multistage") {
  # Next download the information regarding the expertise of the readers
  expdatjson <- fromJSON(file=paste0("https://smartdots.ices.dk/api/Reporting/getReportingVariablesRandom?tblEventID=", config$event_id))
  
  nrows <- length(expdatjson)
  namecols <- names(unlist(expdatjson[[1]]))
  ncols <- length(namecols)
  
  expdat <- matrix(NA, ncol=ncols, nrow=nrows)
  colnames(expdat) <- namecols
  
  for(i in 1:nrows)
  {
    expdat[i,]= unlist(expdatjson[i])  
  }
  
  write.csv(expdat, "expdat.csv", row.names = FALSE)
  
}


