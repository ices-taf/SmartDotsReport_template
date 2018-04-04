## Preprocess data, write TAF data tables

## Before:
## After: report_template.docx, dist.csv,
##        ad_long.csv, ad_long_ex.csv,
##        ad_wide.csv, ad_wide_ex.csv

library(icesTAF)
library(jsonlite)

# create data directory
mkdir("data")

# get utility functions
# I think this should be a smartdotsReport package...
source("utilities-smartdots.R")

# load configuration
config <- read_json("config.json", simplifyVector = TRUE)

# download the report template
download.file(url = config$report_template, "data/reportTemplate.docx", quiet = TRUE, mode = "wb")

# get data from database




# Get data
ad <- read.csv("../2018_smartDotsReportData/output/data.csv", stringsAsFactors = FALSE, header = TRUE)
dist <- read.csv("../2018_smartDotsReportData/output/dist.csv", stringsAsFactors = FALSE, header = TRUE)

# Calculate modal ages and create main data structures ########################

# long format with one reader column
ad_long <- make_data1(ad, "All", "Mode")
ad_long_ex <- make_data1(ad, "Expert", "Mode")

# wide format with one column per reader
ad_wide <- make_data2(ad_long)
ad_wide_ex <- make_data2(ad_long_ex)

# write out input data tables
write.taf(dist, "data/dist.csv")
write.taf(ad_long, "data/ad_long.csv")
write.taf(ad_long_ex, "data/ad_long_ex.csv")
write.taf(ad_wide, "data/ad_wide.csv")
write.taf(ad_wide_ex, "data/ad_wide_ex.csv")
