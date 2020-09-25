#' configuration settings file
#'
#' @name config
#' @format json files
#' @tafOriginator WGSMART
#' @tafYear 2020
#' @tafAccess Public
#' @tafSource script

library(icesTAF)
library(jsonlite)

# Load and edit the json script to adapt to the event

config <- read_json(taf.boot.path("initial", "data", "config.json"), simplifyVector = TRUE)

if (!nzchar(config$ma_method)) {
  config$ma_method <- "Mode"
}

fill <- function(x, string) {
  if (nzchar(x)) {
    x
  } else {
    paste0(string, config$event_id)
  }
}

config$summary_name <-
  fill(config$summary_name, "SmartDots_Summary_Event_")

config$summary_title <-
  fill(config$summary_title, "SmartDots Summary for event ")

config$report_name <-
  fill(config$report_name, "SmartDots_Report_Event_")

config$report_title <-
  fill(config$report_title, "SmartDots Report for event ")

write_json(config, "config.json", simplifyVector = TRUE, pretty = TRUE, flatten = TRUE)
