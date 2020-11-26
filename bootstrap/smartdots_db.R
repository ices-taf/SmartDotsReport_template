#' Script to download data from a SmartDots event
#'
#' @name smartdots_db
#' @format csv files
#' @tafOriginator WGSMART
#' @tafYear 2020
#' @tafAccess Public
#' @tafSource script

## Before:
## After: report_template.docx, dist.csv,
##        ad_long.csv, ad_long_ex.csv,
##        ad_wide.csv, ad_wide_ex.csv

library(icesTAF)
library(jsonlite)
taf.library(icesConnect)
library(httr)

# load configuration
config <- read_json(taf.data.path("config", "config.json"), simplifyVector = TRUE)

# get data from api --------
data <-
  httr::content(
    icesConnect::ices_get_jwt(
      paste0("https://smartdots.ices.dk/API/getReportAnnotations/", config$event_id)
    ),
    simplifyVector = TRUE
  )

dist <-
  httr::content(
    icesConnect::ices_get_jwt(
      paste0("https://smartdots.ices.dk/API/getReportDotsDistances/", config$event_id)
    ),
    simplifyVector = TRUE
  )

# drop comments feild
ad <- ad[, names(ad) != "comment"]

# write out 'bootstrap' data tables
write.taf(dist, quote = TRUE)
write.taf(data, quote = TRUE)
