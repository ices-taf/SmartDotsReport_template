## Prepare plots/tables for report

## Before:
## After:

# load libraries
library(icesTAF)
library(rmarkdown)

# source utilities
source("utilities-smartdots.R")

# load configuration data
load.config("config.json")

# load data for report
dist <- read.taf("data/dist.csv")

# get csv files
for (file in dir("output", pattern = "*.csv")) {
  assign(gsub(".csv", "", file),
         read.taf(paste0("output/", file))
  )
}
# now rdata files
for (file in dir("output", pattern = "*.rData")) {
  load(paste0("output/", file))
}

# render report
mkdir("report")
render("report.Rmd",
       params = list(report_title = report_title),
       output_dir = "report",
       output_file = paste0(report_name, ".docx"),
       encoding = "UTF-8")

