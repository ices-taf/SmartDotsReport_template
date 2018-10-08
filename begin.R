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

# data: one row per set of dots
msg("downloading annotations for ... ", filter)
sqlq <- sprintf(paste("select * FROM vw_report_Annotations where %s"), filter)
conn <- odbcDriverConnect(connection = dbConnection)
ad <- sqlQuery(conn, sqlq)
odbcClose(conn)

# dist: one row per dot
msg("downloading dots for ... ", filter)
sqlq <- sprintf(paste("select * FROM vw_report_DotsDistances where %s"), filter)
conn <- odbcDriverConnect(connection = dbConnection)
dist <- sqlQuery(conn, sqlq)
odbcClose(conn)

# write out 'begin' data tables
write.taf(dist, "begin/dist.csv")
write.taf(ad, "begin/data.csv")
