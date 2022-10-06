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
library(reshape2)


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
ad_long_ex <- read.taf("data/ad_long_ex.csv")
expdat <- read.taf("data/reader_expertise_weighting_data.csv")


# set strata to NULL is all are NA
if (all(is.na(ad_long_all[["strata"]]))) config$strata <- NULL

# get csv files
for (file in dir("model", pattern = "*.csv")) {
  assign(
    gsub(".csv", "", file),
    read.taf(paste0("model/", file))
  )
}
# now rdata files
for (file in dir("model", pattern = "*.rds")) {
  assign(
    tools::file_path_sans_ext(file),
    readRDS(paste0("model/", file))
  )
}

##########################################
# render summary and copy to report folder

########################
# First, check that there are no missing data in stock, ices_area and prep_method in the ad_long, ad_long_ex or expdat data. Also check that there are no missing values in any of the variables used to assess the expertise of readers

if (any(c(any(ad_long$prep_method == "missing"), any(ad_long$stock == "missing"), any(ad_long$ices_area == "missing")) == TRUE)) {
  config$error_message_I <- "Error: There have been found missing data in the annotations data in one or more of variables: prep_method, stock and ices_area. Please, make sure that the required information has been provided to all fields"
}

if (config$mode_definition == "multistage") {
  if (any(c(any(expdat$preparation_Method == "missing"), any(expdat$stockCode == "missing"), any(expdat$iceS_Area == "missing")) == TRUE)) {
    config$error_message_II <- "Error: There have been found missing data in the reader expertise data provided to SmartDots in one or more  of variables: preparation_Method, stockCode or ices_area. Please, make sure that the required information has been provided to all fields"
  }

  if (any(c(any(expdat$noYearsCurrent == "missing"), any(expdat$meanOtoCurrent == "missing"), any(expdat$totalNumberOFftolitsReadCurrent == "missing"), any(expdat$noYearsReading == "missing")) == TRUE)) {
    config$error_message_III <- "Error: There have been found missing data in the reader expertise data provided to SmartDots in one or more of variables: noYearsCurrent, meanOtoCurrent, totalNumberOFftolitsReadCurrent and/or noYearsReading. Please, make sure that the required information has been provided to all fields"
  }

  # Find those cases in the annotations file for which there is not information on readers expertise.
  comb_ad_long <- plyr::ddply(ad_long, .(stock, ices_area, prep_method, reader_number), summarise, count = length(stock))
  comb_expdat <- read.taf("data/reader_expertise_weighting_data.csv")
  comb_expdat <- comb_expdat[, colnames(comb_expdat) %in% c("stockCode", "iceS_Area", "preparation_Method", "rank_readers")]
  colnames(comb_expdat) <- c("stock", "prep_method", "ices_area", "reader_number")
  comb_ad_long$code <- paste0(comb_ad_long$stock, "---", comb_ad_long$ices_area, "---", comb_ad_long$prep_method, "---", comb_ad_long$reader_number)
  comb_expdat$code <- paste0(comb_expdat$stock, "---", comb_expdat$ices_area, "---", comb_expdat$prep_method, "---", comb_expdat$reader_number)
  difference <- setdiff(comb_ad_long$code, comb_expdat$code)
  if (length(difference) > 0) {
    config$error_message_IV <- "Error: There are some combinations of stock---ices_area---prep_method---reader_number for which the information on age reading expertise is missing. Please, make sure that the required information has been provided to all fields. These are cases for which that information is missing: "
    config$error_message_V <- paste0(as.character(difference), " ; ")
  }
}

#####################################
# If there is missing information in the annotations or the readers expertise data, print the failure report with the error messages indicating the missing information. If there is no missing information print the summary and full reports.

if (any(config$error_message_I != "", config$error_message_II != "", config$error_message_III != "", config$error_message_IV != "", config$error_message_V != "")) {
  failure_report_filename <- paste0(config$report_name, "_ERROR.docx")
  render("report_failure_message.Rmd",
    params = list(
      report_title = config$report_title,
      error_message_I = config$error_message_I,
      error_message_II = config$error_message_II,
      error_message_III = config$error_message_III,
      error_message_IV = config$error_message_IV,
      error_message_V = config$error_message_V
    ),
    output_file = failure_report_filename,
    encoding = "UTF-8"
  )
  cp(failure_report_filename, "report", move = TRUE)
} else {

  # the summary and full reports
  summary_filename <- paste0(config$summary_name, ".docx")
  if (is.null(config$strata)) {
    render("report_summary.Rmd",
      params = list(
        summary_title = config$summary_title,
        warn_AEM = config$warn_AEM
      ),
      output_file = summary_filename,
      encoding = "UTF-8"
    )
  } else {
    render("report_summary.Rmd",
      params = list(
        summary_title = config$summary_title,
        strata = unique(ad_long_all$strata),
        warn_AEM = config$warn_AEM
      ),
      output_file = summary_filename,
      encoding = "UTF-8"
    )
  }
  cp(summary_filename, "report", move = TRUE)

  # render report and copy to report folder
  report_filename <- paste0(config$report_name, ".docx")
  if (is.null(config$strata)) {
    render("report_full.Rmd",
      params = list(
        report_title = config$report_title,
        warn_AEM = config$warn_AEM
      ),
      output_file = report_filename,
      encoding = "UTF-8"
    )
  } else {
    render("report_full.Rmd",
      params = list(
        report_title = config$report_title,
        strata = unique(ad_long_all$strata),
        warn_AEM = config$warn_AEM
      ),
      output_file = report_filename,
      encoding = "UTF-8"
    )
  }
  cp(report_filename, "report", move = TRUE)
}


# copy disclaimer into report folder
cp("bootstrap/data/Disclaimer.txt", "report")
