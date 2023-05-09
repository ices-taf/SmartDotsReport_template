
library(icesTAF)
library(jsonlite)

if (FALSE) {
  # set current WD if required
  current_dir <- rstudioapi::getActiveDocumentContext()$path
  setwd(dirname(current_dir))
  getwd()
}

# Define some of the arguments in the json file:
event_number <- 449
OnlyApproved <- TRUE
tokens <- "tokens goes here"
mode_definition <- "multistage" # it must be set as "multistage" or "standard" the default way is multistage approach, that can be changed to the "standard" mode calculation.

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
    strata = "strata" # strata defined in smartdots reporting web page
  )

write_json(
  config, "bootstrap/initial/data/config.json",
  pretty = TRUE
)

# install missing dependencies
install.deps()

# fetch data
taf.bootstrap(taf = TRUE)

# either run each in turn:
if (FALSE) {
  sourceTAF("data")
  sourceTAF("model")
  sourceTAF("report")
}

# or all at once
sourceAll()
