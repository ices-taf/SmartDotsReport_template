## Preprocess data, write TAF data tables

## Before:
## After: report_template.docx, dist.csv,
##        ad_long.csv, ad_long_ex.csv,
##        ad_wide.csv, ad_wide_ex.csv

library(icesTAF)
library(jsonlite)
library(dplyr)
library(tidyr)
require(lubridate)

# create data directory
mkdir("data")

# get utility functions
source("utilities.R")
source("utilities_data.R")

# load configuration
config <- read_json("bootstrap/data/config.json", simplifyVector = TRUE)

# get data from bootstrap folder  -------------------------------

ad <- read.taf("bootstrap/data/smartdots_db/data.csv")
dist <- read.taf("bootstrap/data/smartdots_db/dist.csv")

# prepare data -------------------------------

# keep only approved annotations
if (config$onlyApproved) {
  ad <- ad[ad$IsApproved == 1, ]
  dist <- dist[dist$IsApproved == 1, ]
}

# add date columns
ad <-
  within(ad, {
    year = lubridate::year(parse_date_time(catch_date, '%d/%m/%Y %H:%M:%S'))
    qtr = lubridate::quarter(parse_date_time(catch_date, '%d/%m/%Y %H:%M:%S'))
    month = lubridate::month(parse_date_time(catch_date, '%d/%m/%Y %H:%M:%S'))
  })

# if variables are missing add "missing"
ad$ices_area[is.na(ad$ices_area) | ad$ices_area == ""] <- "missing"
ad$stock[is.na(ad$stock) | ad$stock == ""] <- "missing"
ad$prep_method[is.na(ad$prep_method) | ad$prep_method == ""] <- "missing"

# if variables are missing add "missing"
dist$ices_area[is.na(dist$ices_area) | dist$ices_area == ""] <- "missing"


# if no advanced readers! make them all advanced
if (all(ad$expertise == 0)) {
  msg("NOTE: all readers were Basic - all have been converted to Advanced")
  ad$expertise[] <- 1
}

# convert reader expertise
ad$expertise <- c("Basic", "Advanced")[ad$expertise + 1]

# Calculate modal ages and cv of modal age
ad_long <- add_modalage(ad, config$ma_method)
ad_long_ex <- add_modalage(ad[ad$expertise == "Advanced", ], config$ma_method)


# prepare data in wbgr output format
# IMAGE,1,2,3,4,5,6,7,8,9,10,11,12,13
# Expertise level,-,-,-,-,-,-,-,-,-,-,-,-,-
# Stock assessment,no,no,no,no,no,no,no,no,no,no,no,no,no
# 6698256.jpg,1,1,1,1,1,-,2,1,-,2,-,1,-
# 6698257.jpg,3,3,3,3,2,1,3,3,-,3,-,3,-
readers <- sort(unique(ad$reader))
webgr <- spread(ad_long[c("sample", "reader", "age")], key = reader, value = age)
webgr[] <- paste(unlist(webgr))
webgr[webgr == "NA"] <- "-"
webgr <-
  rbind(c("Expertise level", rep("-", length(readers))),
        c("Stock assessment", rep("no", length(readers))),
        webgr)
names(webgr) <- c("IMAGE", 1:length(readers))
head(webgr)


# write out input data tables for use later
write.taf(dist, "data/dist.csv", quote = TRUE)
write.taf(ad, "data/data.csv", quote = TRUE)
write.taf(ad_long, "data/ad_long.csv", quote = TRUE)
write.taf(ad_long_ex, "data/ad_long_ex.csv", quote = TRUE)
write.taf(webgr, "data/WebGR_ages_all.csv", quote = TRUE)
