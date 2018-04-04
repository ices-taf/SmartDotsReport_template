## Prepare plots/tables for report

## Before:
## After:

# load libraries
library(icesTAF)
library(rmarkdown)

# source utilities
source("utilities-smartdots.R")

# load configuration data
config <- read_json("config.json", simplifyVector = TRUE)

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
       output_dir = "report",
       params = list(report_title = config$report_title),
       output_file = paste0(config$report_name, ".docx"),
       encoding = "UTF-8")

