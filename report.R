## Prepare plots/tables for report

## Before:
## After:

# load libraries
library(icesTAF)
library(rmarkdown)
library(jsonlite)
library(knitr)

library(pander)
library(ggplot2)
library(scales)
library(dplyr)
require(Hmisc)

# make report directory
mkdir("report")

# source utilities
source("utilities.R")
source("utilities_report.R")

# load configuration data
config <- read_json("bootstrap/data/config.json", simplifyVector = TRUE)

# load data for report
dist <- read.taf("data/dist.csv")
ad_long_all <- read.taf("data/ad_long.csv")
ad_long_adv <- read.taf("data/ad_long_adv.csv")

# set strata to NULL is all are NA
if (all(is.na(ad_long_all[["strata"]]))) config$strata <- NULL

# get csv files
for (file in dir("model", pattern = "*.csv")) {
  assign(gsub(".csv", "", file),
         read.taf(paste0("model/", file))
  )
}
# now rdata files
for (file in dir("model", pattern = "*.rds")) {
  assign(tools::file_path_sans_ext(file),
         readRDS(paste0("model/", file))
  )
}

# render summary and copy to report folder
summary_filename <- paste0(config$summary_name, ".docx")
if(is.null(config$strata)) {
        render("report_summary.Rmd",
               params = list(summary_title = config$summary_title),
               output_file = summary_filename,
               encoding = "UTF-8")
} else {
        render("report_summary.Rmd",
               params = list(summary_title = config$summary_title,
                             strata = unique(ad_long_all$strata)),
               output_file = summary_filename,
               encoding = "UTF-8")
}
cp(summary_filename, "report", move = TRUE)

# render report and copy to report folder
report_filename <- paste0(config$report_name, ".docx")
if(is.null(config$strata)) {
        render("report_full.Rmd",
               params = list(report_title = config$report_title),
               output_file = report_filename,
               encoding = "UTF-8")
} else {
        render("report_full.Rmd",
               params = list(report_title = config$report_title,
                             strata = unique(ad_long_all$strata)),
               output_file = report_filename,
               encoding = "UTF-8")
}
cp(report_filename, "report", move = TRUE)

# move disclaimer to report folder
cp("bootstrap/data/Disclaimer.txt", "report", move = TRUE)
