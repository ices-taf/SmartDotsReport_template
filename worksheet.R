
library(icesTAF)
library(jsonlite)

# step 0: mimic server and create config file

# create config as named list
server.config <-
  list(
    event_id = unbox(413),
    token = unbox("3F482827-282B-46E0-8C58-FBCF6C4B2C55"),
    mode_definition = "multistage",
    ma_method = unbox("Mode"),
    strata = unbox("strata"),
    onlyApproved = unbox(TRUE),
    summary_name = unbox("SmartDots_Summary"),
    summary_title = unbox("SmartDots Summary"),
    report_name = unbox("SmartDots_Report"),
    report_title = unbox("SmartDots Report"),
    report_tokens = unbox("token_goes_here")
  )

# write to initial data
cat(
  toJSON(server.config, pretty = TRUE),
  file = taf.boot.path("initial", "data", "config.json")
)

# step 1: run bootstrap

taf.bootstrap(taf = TRUE)

# step 2: run taf scripts

#sourceAll()

sourceTAF("data")
