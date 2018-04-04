## Preprocess data, write TAF data tables

## Before:
## After: report_template.docx, dist.csv,
##        ad_long.csv, ad_long_ex.csv,
##        ad_wide.csv, ad_wide_ex.csv

library(icesTAF)
library(jsonlite)
library(RODBC)

# create data directory
mkdir("data")

# get utility functions
# I think this should be a smartdotsReport package...
source("utilities-smartdots.R")

# load configuration
config <- read_json("config.json", simplifyVector = TRUE)

# get data from database

# DB settings
dbConnection <- 'Driver={SQL Server};Server=SQL06;Database=SmartDots;Trusted_Connection=yes'

# data: one row per set of dots
filter <- paste(sprintf("EventID = %i", config$event_ids), collapse = " or ")
sqlq <- sprintf(paste("select * FROM vw_report_Annotations where %s"), filter)

# connect to DB and fetch
msg("downloading annotations for ... ")
conn <- odbcDriverConnect(connection = dbConnection)
ad <- sqlQuery(conn, sqlq)
odbcClose(conn)

ad[ad$sample == "Npout_054_whole",]


# dist: one row per dot
# reader1, sample, reader_number, mark, distance, ices_area, reader
# R01, 6698256.jpg, 1, 1, 0, IIIaS, R01 GBR
# set up sql command
filter <- "tblEventID = 74 or tblEventID = 77"
sqlq <- sprintf(paste("select * FROM vw_report_DotsDistances where %s"), filter)

# connect to DB and fetch
msg("downloading dots for ... ")
conn <- odbcDriverConnect(connection = dbConnection)
dist <- sqlQuery(conn, sqlq)
odbcClose(conn)

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
