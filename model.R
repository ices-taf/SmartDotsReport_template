## Run analysis, write model results

## Before: data/ad_long.csv, data/ad_long_ex.csv,
##         data/ad_wide.csv, data/ad_wide_ex.csv
## After:

library(icesTAF)
library(jsonlite)
unloadNamespace("dplyr")
library(plyr) # age error matrix
library(dplyr)
library(tidyr)
library(tibble) # bias_test

library(ggplot2)
library(scales) # rescale_none

# make model directory
mkdir("model")

# load configuration
config <- read_json("config.json", simplifyVector = TRUE)

# load utilities
source("utilities.R")
source("utilities_model.R")

# read input data
ad_long_all <- read.taf("data/ad_long.csv")
ad_long_ex <- read.taf("data/ad_long_ex.csv")

# model age range
modal_age_range <-  with(ad_long_all, min(modal_age, na.rm = TRUE):max(modal_age, na.rm = TRUE))

# set strata to NULL is all are NA
if (all(is.na(ad_long_all[[config$strata]]))) config$strata <- NULL

# Overview of samples and readers ##############################################

# Sample overview
sample_data_overview <- sample_data_overview_table(ad_long_all, config$strata)
write.taf(sample_data_overview, dir = "model")

# Participants table
reader_data <-
  ad_long_all %>%
  select(reader, expertise) %>%
  unique %>%
  arrange(reader) %>%
  rename(`Reader code` = reader,
         Expertise = expertise)

write.taf(reader_data, dir = "model")


# Results ##############################################

# repeat for all and for experts only

for (group in c("all", "ex")) {
# group <- "all"

  # get the appropriate dataset
  ad_long <- get(vname("ad_long"))

  # number read table
  assign(
    vname("num_read_tab"),
    num_read_table(ad_long, by = "reader")
  )
  write.taf(vname("num_read_tab"), dir = "model")

  # CV table
  assign(
    vname("cv_tab"),
    cv_table(ad_long, by = "reader")
  )
  write.taf(vname("cv_tab"), dir = "model")

  # APE table
  assign(
    vname("ape_tab"),
    ape_table(ad_long, by = "reader")
  )
  write.taf(vname("ape_tab"), dir = "model")


  # Percent agreement between age readings and modal age.
  assign(
    vname("pa_tab"),
    pa_table(ad_long, by = "reader")
  )
  write.taf(vname("pa_tab"), dir = "model")


  # Relative bias
  assign(
    vname("rel_bias_tab"),
    rel_bias_table(ad_long, by = "reader")
  )
  write.taf(vname("rel_bias_tab"), dir = "model")

  # Inter reader bias test - TODO clean bais_test function
  assign(
    vname("bias_tab"),
    {
      x <- bias_test(ad_long)
      x[is.na(x)] <- "-"
      x
    }
  )
  write.taf(vname("bias_tab"), dir = "model")

  # Results by strata ##############################################

  # loop over strata - 4 tables per strata
  # stratum = "prep_method"
  for (stratum in config$strata) {

    # Calculate number of readings per reader grouped by modal age and add total
    assign(vsname("num_read_tab"),
      num_read_table(ad_long, by = stratum)
    )
    write.taf(vsname("num_read_tab"), dir = "model")

    # CV table
    assign(vsname("cv_tab"),
      cv_table(ad_long, by = stratum)
    )
    write.taf(vsname("cv_tab"), dir = "model")

    # Percent agreement between age readings and modal age.
    assign(vsname("pa_tab"),
      pa_table(ad_long, by = stratum)
    )
    write.taf(vsname("pa_tab"), dir = "model")

    # Relative bias
    assign(vsname("rel_bias_tab"),
      rel_bias_table(ad_long, by = stratum)
    )
    write.taf(vsname("rel_bias_tab"), dir = "model")

  }

  ## Annex tables

  # data overview - TODO fix overveiw table
  assign(
    vname("data_overview_tab"),
    data_overview_table(ad_long, config$report_tokens)
  )
  write.taf(vname("data_overview_tab"), dir = "model")

  # Age composition
  assign(
    vname("age_composition_tab"),
    age_composition_table(ad_long, by = "reader")
  )
  write.taf(vname("age_composition_tab"), dir = "model")

  # Mean length at age
  assign(
    vname("mean_length_tab"),
    mean_length_table(ad_long, by = "reader")
  )
  write.taf(vname("mean_length_tab"), dir = "model")

  # Age error matrix (AEM) only for advanced readers
  assign(
    vname("ae_mat"),
    age_er_matrix(ad_long, by = c("ices_area", "stock"))
  )
  # todo - do a better job of this...
  saveRDS(get(vname("ae_mat")),
          file = file.path("model", paste0(vname("ae_mat"), ".rds")))

}
