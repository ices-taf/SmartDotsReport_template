## Check, preprocess data, write TAF data tables

library(icesTAF)

# create data directory
mkdir("data")

sourceTAF("data_checker.R")
sourceTAF("data_processing.R")

# done
