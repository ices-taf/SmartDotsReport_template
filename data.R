## Preprocess data, write TAF data tables

## Before:
## After: report_template.docx, dist.csv,
##        ad_long.csv, ad_long_ex.csv,
##        ad_wide.csv, ad_wide_ex.csv

library(icesTAF)
library(jsonlite)
library(RODBC)
library(tidyr)

# create data directory
mkdir("data")

# get utility functions
# I think this should be a smartdotsReport package...
source("utilities_data.R")
source("utilities.R")

# load configuration
config <- read_json("config.json", simplifyVector = TRUE)

# get data from database

# DB settings
dbConnection <- 'Driver={SQL Server};Server=SQL06;Database=SmartDots;Trusted_Connection=yes'

# create filter for events
filter <- paste(sprintf("EventID = %i", config$event_ids), collapse = " or ")

# update view
conn <- odbcDriverConnect(connection = dbConnection)
sqlq <- paste(readLines("utilities_vw_report_Annotations.sql"), collapse = "\n")
sqlQuery(conn, sqlq)
odbcClose(conn)

# data: one row per set of dots
msg("downloading annotations for ... ", filter)
sqlq <- sprintf(paste("select * FROM vw_report_Annotations where %s"), filter)
conn <- odbcDriverConnect(connection = dbConnection)
ad <- sqlQuery(conn, sqlq)
odbcClose(conn)

# prepare data
ad <-
  within(ad, {
    year = lubridate::year(catch_date)
    qtr = lubridate::quarter(catch_date)
    month = lubridate::month(catch_date)
  })

# Calculate modal ages and cv of modal age
ad_long <- add_modalage(ad, config$ma_method)
ad_long_ex <- add_modalage(ad[ad$expertise == 1, ], config$ma_method)

# Create wide data with one row per sample.
# Reader will be used as new column headings for the age readings.
var_in <- c("sample", "length", "sex", "catch_date", "ices_area",
             "year", "qtr","month","reader", "age", "cv", "modal_age")

ad_wide <- spread(ad_long[var_in], key = reader, value = age)
ad_wide_ex <- spread(ad_long_ex[var_in], key = reader, value = age)


# write out input data tables
write.taf(ad, "data/data.csv")
write.taf(ad_long, "data/ad_long.csv")
write.taf(ad_long_ex, "data/ad_long_ex.csv")
write.taf(ad_wide, "data/ad_wide.csv")
write.taf(ad_wide_ex, "data/ad_wide_ex.csv")




# update view
conn <- odbcDriverConnect(connection = dbConnection)
sqlq <- paste(readLines("utilities_vw_report_DotsDistances.sql"), collapse = "\n")
sqlQuery(conn, sqlq)
odbcClose(conn)

# dist: one row per dot
msg("downloading dots for ... ", filter)
sqlq <- sprintf(paste("select * FROM vw_report_DotsDistances where %s"), filter)
conn <- odbcDriverConnect(connection = dbConnection)
dist <- sqlQuery(conn, sqlq)
odbcClose(conn)

# write out input data tables
write.taf(dist, "data/dist.csv")
