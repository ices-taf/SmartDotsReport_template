## Preprocess data, write TAF data tables

## Before:
## After: report_template.docx, dist.csv,
##        ad_long.csv, ad_long_ex.csv,
##        ad_wide.csv, ad_wide_ex.csv

library(icesTAF)
library(jsonlite)
library(plyr)
library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)

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

# check or create the stratification here
if (is.null(config$strata) || is.na(config$strata)) {
  ad$strata = NA
} else {
  ad$strata = apply(as.data.frame(ad[, config$strata]), 1, function(x) {
    paste(str_to_title(config$strata), x, sep = "_", collapse = "_and_")
  })
}



estratos <- plyr::ddply(ad, .(SampleID), summarise, strata = unique(strata))
dim(dist)
dist <- dist[,!colnames(dist) %in% c("strata")]
dist <- merge(dist, estratos, by.x="SampleID", by.y="SampleID")
dim(dist)


# if no advanced readers! make them all advanced
if (all(ad$expertise == 0)) {
  msg("NOTE: all readers were Basic - all have been converted to Advanced")
  ad$expertise[] <- 1
}

# convert reader expertise
ad$expertise <- c("Basic", "Advanced")[ad$expertise + 1]

# Assign weight to the readers based in their ranking-experience
weight <- length(sort(unique(ad$reader_number))):1
reader_number <- sort(unique(ad$reader_number))
reader <- data.frame(reader_number = reader_number, weight_I = weight, weight_II = 1 / (1 + log(sort(weight, decreasing = FALSE) + 0.0000000001)))
ad <- merge(ad, reader, by.x = "reader_number", by.y = "reader_number", all.x = TRUE)


# Calculate modal ages and cv of modal age
#message("perhaps change modal calculation functions to better name columns 0, 1, 2 ...")
ad_long <- ad %>%
  add_modal_trad(config$ma_method) %>%
  add_modal_linearweight(config$ma_method) %>%
  add_modal_negexpweight(config$ma_method)

ad_long_adv <- ad[ad$expertise == "Advanced", ] %>%
  add_modal_trad(config$ma_method) %>%
  add_modal_linearweight(config$ma_method) %>%
  add_modal_negexpweight(config$ma_method)

# Choose the final mode (traditional, readers linear weight or negative exponential linear weight) based in the existence of multimodality or not.
ad_long <- select_mode(ad_long, config$ma_method, config$mode_definition)
ad_long_adv <- select_mode(ad_long_adv, config$ma_method, config$mode_definition)

# prepare data in wbgr output format
# IMAGE,1,2,3,4,5,6,7,8,9,10,11,12,13
# Expertise level,-,-,-,-,-,-,-,-,-,-,-,-,-
# Stock assessment,no,no,no,no,no,no,no,no,no,no,no,no,no
# 6698256.jpg,1,1,1,1,1,-,2,1,-,2,-,1,-
# 6698257.jpg,3,3,3,3,2,1,3,3,-,3,-,3,-
readers <- sort(unique(ad$reader))
webgr <- spread(ad[c("sample", "reader", "age")], key = reader, value = age)
webgr[] <- paste(unlist(webgr))
webgr[webgr == "NA"] <- "-"
webgr <-
  rbind(c("Expertise level", rep("-", length(readers))),
        c("Stock assessment", rep("no", length(readers))),
        webgr)
names(webgr) <- c("IMAGE", 1:length(readers))
head(webgr)

# write out input data tables for use later
write.taf(dist, dir = "data", quote = TRUE)
write.taf(ad, file = "data.csv", dir = "data", quote = TRUE)
write.taf(ad_long, dir = "data", quote = TRUE)
write.taf(ad_long_adv, dir = "data", quote = TRUE)
write.taf(webgr, file = "WebGR_ages_all.csv", dir = "data", quote = TRUE)
