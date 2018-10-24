## Preprocess data, write TAF data tables

## Before:
## After: report_template.docx, dist.csv,
##        ad_long.csv, ad_long_ex.csv,
##        ad_wide.csv, ad_wide_ex.csv

library(icesTAF)
library(jsonlite)

# create data directory
mkdir("bootstrap/downloads")

# load configuration
config <- read_json("config.json", simplifyVector = TRUE)

# get data from api --------
zipfile <- "bootstrap/downloads/smartdots_data.zip"
download.file(paste0("https://smartdots.ices.dk/download/DownloadEvent.ashx?tblEventID=", config$event_id),
              zipfile,  mode = "wb")
files <- unzip(zipfile, list = TRUE)$Name
files <- files[grep("*.csv", files)]
unzip(zipfile, files = files, exdir = "bootstrap/downloads")

# read in and write out again
dist <- read.csv(paste0("bootstrap/downloads/", files[grep("DotsDistances",  files)]), stringsAsFactors = FALSE)
ad <- read.csv(paste0("bootstrap/downloads/", files[grep("Annotations",  files)]), stringsAsFactors = FALSE)

# convert True and False to true and false
ad$IsApproved <- ad$IsApproved == "True"
dist$IsApproved <- dist$IsApproved == "True"



# hacks!!!!

library(dplyr)
library(tidyr)

 multiple_annotations <-
    ad %>%
    group_by(FishID, reader) %>%
    count() %>%
    filter(n > 1) %>%
    rename(annotations = n)

 for (i in seq_along(multiple_annotations$annotations)) {
   iage <- ad$age[which(ad$FishID == multiple_annotations$FishID[i] & ad$reader == multiple_annotations$reader[i])]
   drop <- seq_along(iage)[-which.max(iage)]
   ad <- ad[-which(ad$FishID == multiple_annotations$FishID[i] & ad$reader == multiple_annotations$reader[i])[drop],]
 }

# remove dots from dropped annotations
dist <- dist[dist$AnnotationID %in% ad$AnnotationID,]

# remove annotations on (and very near) the centre
dist <- dist[dist$pixelDistance > 2,]

# adjust ages in age data from removing dots close to centre
ad$age <- unname(table(factor(dist$AnnotationID, levels = ad$AnnotationID))[paste(ad$AnnotationID)])


# end hacks  !!!


# write out 'bootstrap' data tables
write.taf(dist, "bootstrap/dist.csv")
write.taf(ad, "bootstrap/data.csv")
