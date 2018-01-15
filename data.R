## Preprocess data, write TAF data tables

## Before:
## After:

library(icesTAF)

# get utility functions
source("utilities.R")
# I think this should be a smartdotsReport package...
source("utilities-smartdots.R")

# load configuration
load.config("config.json")

# download raw data
download.raw(folders$raw$repo, folders$raw$ref)

# Get and prepare data ########################################################

# Get data
ad <- read.csv("raw/data.csv", stringsAsFactors = FALSE, header = TRUE)
dist <- read.csv("raw/dist.csv", stringsAsFactors = FALSE, header = TRUE)

# Calculate modal ages and create main data structures ########################

# long format with one reader column
ad_long <- make_data1(ad, "All", "Mode")
ad_long_ex <- make_data1(ad, "Expert", "Mode")


# wide format with one column per reader
ad_wide <- make_data2(ad_long)
ad_wide_ex <- make_data2(ad_long_ex)


# write out input data tables
mkdir("data")
write.taf(dist, "data/dist.csv")
write.taf(ad_long, "data/ad_long.csv")
write.taf(ad_long_ex, "data/ad_long_ex.csv")
write.taf(ad_wide, "data/ad_wide.csv")
write.taf(ad_wide_ex, "data/ad_wide_ex.csv")
