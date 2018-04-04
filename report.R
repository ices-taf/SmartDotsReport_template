## Prepare plots/tables for report

## Before:
## After:

# load libraries
library(icesTAF)
library(rmarkdown)

# make report directory
mkdir("report")

# source utilities
source("utilities-smartdots.R")

# load configuration data
config <- read_json("config.json", simplifyVector = TRUE)

# load data for report
dist <- read.taf("data/dist.csv")

# get csv files
for (file in dir("model", pattern = "*.csv")) {
  assign(gsub(".csv", "", file),
         read.taf(paste0("model/", file))
  )
}
# now rdata files
for (file in dir("model", pattern = "*.rData")) {
  load(paste0("model/", file))
}

# render report and copy to report folder
report_filename <- paste0(config$report_name, ".docx")
render("report.Rmd",
       params = list(report_title = config$report_title),
       output_file = report_filename,
       encoding = "UTF-8")
cp(report_filename, "report", move = TRUE)

# finally copy to sharepoint?
