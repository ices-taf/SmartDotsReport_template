## Check, preprocess data, write TAF data tables

library(icesTAF)

# source bootstrap!
source("bootstrap.R")

# create data directory
mkdir("data")

sourceTAF("data_checker.R")
sourceTAF("data_processing.R")

# done

