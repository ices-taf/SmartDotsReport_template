## Preprocess data, write TAF data tables

## Before:
## After: report_template.docx, dist.csv,
##        ad_long.csv, ad_long_ex.csv,
##        ad_wide.csv, ad_wide_ex.csv

library(icesTAF)
library(jsonlite)
library(RODBC)

# create data directory
mkdir("begin")

# load configuration
config <- read_json("config.json", simplifyVector = TRUE)

# get data from database -------------------------------

# DB settings
dbConnection <- 'Driver={SQL Server};Server=SQL06;Database=SmartDots;Trusted_Connection=yes'

# create filter for events
filter <- paste(sprintf("EventID = %i", config$event_ids), collapse = " or ")

# update view
msg("updating annotate view")
conn <- odbcDriverConnect(connection = dbConnection)
sqlq <- paste(readLines("utilities_vw_report_Annotations.sql"), collapse = "\n")
ret <- sqlQuery(conn, sqlq)
odbcClose(conn)
if (length(ret)) msg(ret)

# data: one row per set of dots
msg("downloading annotations for ... ", filter)
sqlq <- sprintf(paste("select * FROM vw_report_Annotations where %s"), filter)
conn <- odbcDriverConnect(connection = dbConnection)
ad <- sqlQuery(conn, sqlq)
odbcClose(conn)


# update view
msg("updating dots distances view")
conn <- odbcDriverConnect(connection = dbConnection)
sqlq <- paste(readLines("utilities_vw_report_DotsDistances.sql"), collapse = "\n")
ret <- sqlQuery(conn, sqlq)
odbcClose(conn)
if (length(ret)) msg(ret)

# dist: one row per dot
msg("downloading dots for ... ", filter)
sqlq <- sprintf(paste("select * FROM vw_report_DotsDistances where %s"), filter)
conn <- odbcDriverConnect(connection = dbConnection)
dist <- sqlQuery(conn, sqlq)
odbcClose(conn)


# write out 'begin' data tables
write.taf(dist, "begin/dist.csv")
write.taf(ad, "begin/data.csv")
