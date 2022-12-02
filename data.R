## Check, preprocess data, write TAF data tables

library(icesTAF)

# Set directory

create_dir=rstudioapi::getActiveDocumentContext()$path
setwd(dirname(create_dir))
getwd()


# create data directory
mkdir("data")

sourceTAF("data_checker.R")
sourceTAF("data_processing.R")

# done

