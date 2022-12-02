
library(icesTAF)
library(jsonlite)

# Define some of the arguments in the json file:
event_number <- 347
OnlyApproved <- TRUE
tokens <- "tokens goes here"
mode_definition <- "multistage" # it must be set as "multistage" or "standard" the default way is multistage approach, that can be changed to the "standard" mode calculation.
select_strata <- "stock_component" # c("stock", "prep_method", "ices_area", "qtr")   # Here the strata can be defined. If we don?t want to produce the report by data stratification, then select_strata has to be set as NULL. However, if we want to split the data by groups (strata) and produce results in the report for each strata, select_strata can be changed for example to "stock", or "ices_area", or "prep_method", or a combination of them. In the script data_processing.R select_strata is used to modify the variable strata in the ad and dist databases.

# create config file in intial data folder
config <-
  list(
    event_id = unbox(event_number),
    ma_method = unbox("Mode"),
    onlyApproved = unbox(OnlyApproved),
    summary_name = unbox(paste0("SmartDots_Summary_Event_", event_number)),
    summary_title = unbox(paste0("SmartDots Summary for event ", event_number)),
    report_name = unbox(paste0("SmartDots_Report_Event_", event_number)),
    report_title = unbox(paste0("SmartDots Report for event ", event_number)),
    report_tokens = unbox(tokens),
    mode_definition = unbox(mode_definition), # the default way is "multistage" approach, that can be changed to the "standard" mode calculation.
    strata = select_strata
  )

write_json(
  config, "bootstrap/initial/data/config.json",
  pretty = TRUE
)

# fetch data
taf.bootstrap(taf = TRUE)
