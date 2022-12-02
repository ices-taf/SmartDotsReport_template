## Preprocess data, write TAF data tables

## Before:
## After: report_template.docx, dist.csv,
##        ad_long.csv, ad_long_ex.csv,
##        ad_wide.csv, ad_wide_ex.csv

rm(list=ls())

library(icesTAF)
library(jsonlite)
library(rjson)

create_dir=rstudioapi::getActiveDocumentContext()$path
setwd(dirname(create_dir))
getwd()


# # load configuration
# config <- read_json("initial/data/config.json", simplifyVector = TRUE)

# Define some of the arguments in the json file:
event_number=347
OnlyApproved <- TRUE
tokens <- "tokens goes here"
mode_definition<- "multistage" # it must be set as "multistage" or "standard" the default way is multistage approach, that can be changed to the "standard" mode calculation.
select_strata <- "stock_component" #c("stock", "prep_method", "ices_area", "qtr")   # Here the strata can be defined. If we don?t want to produce the report by data stratification, then select_strata has to be set as NULL. However, if we want to split the data by groups (strata) and produce results in the report for each strata, select_strata can be changed for example to "stock", or "ices_area", or "prep_method", or a combination of them. In the script data_processing.R select_strata is used to modify the variable strata in the ad and dist databases.


# Load and edit the json script to adapt to the event
config=fromJSON(file="./initial/data/template.json")
config$'event_id'[[1]] <- event_number
config$'onlyApproved'[[1]] <- OnlyApproved
config$'summary_name'[[1]] <- paste0("SmartDots_Summary_Event_", event_number)
config$'summary_title'[[1]] <- paste0("SmartDots Summary for event ", event_number)
config$'report_name'[[1]] <- paste0("SmartDots_Report_Event_", event_number)
config$'report_title'[[1]] <- paste0("SmartDots Report for event ", event_number)
config$'report_tokens'[[1]] <- tokens
config$'mode_definition' <- mode_definition # the default way is "multistage" approach, that can be changed to the "standard" mode calculation.
config$'strata' <- select_strata
saveconfig <- toJSON(config)
write(saveconfig, file="./initial/data/config.JSON")

# get data from api --------
# zipfile <- "smartdots_data.zip"
# download.file(paste0("https://smartdots.ices.dk/download/DownloadEvent.ashx?Token=3F482827-282B-46E0-8C58-FBCF6C4B2C55&tblEventID=", config$event_id),
#               zipfile,  mode = "wb")
#download.file(paste0("https://smartdots.ices.dk/download/DownloadEvent.ashx?Token=505EFEB2-67F7-4711-BE9A-43BDAAB067A8&tblEventID=", config$event_id),
#                     zipfile,  mode = "wb") ##coloquei aqui o meu Token
#  files <- unzip(zipfile, list = TRUE)$Name
#  files <- files[grep("*.csv", files)]
#  unzip(zipfile, files = files, exdir = ".")
# #
# # # read in and write out again
#  dist <- read.csv(files[grep("DotsDistances",  files)], stringsAsFactors = FALSE)
#  ad <- read.csv(files[grep("Annotations",  files)], stringsAsFactors = FALSE)
# #
# # # delete downloaded data
#  unlink(files)
# #
# # # drop comments feild
#  ad <- ad[,names(ad) != "Comment"]
# #
# # # write out 'bootstrap' data tables
#  write.taf(dist, "dist.csv", quote = TRUE)
#  write.taf(ad, "data.csv", quote = TRUE)

