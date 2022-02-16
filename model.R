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
config <- read_json("bootstrap/initial/data/config.json", simplifyVector = TRUE)

# load utilities
source("utilities.R")
source("utilities_model.R")

# read input data
ad_long_all <- read.taf("data/ad_long.csv")
ad_long_ex <- read.taf("data/ad_long_ex.csv")

# model age range
modal_age_range_all <-  with(ad_long_all, min(modal_age, na.rm = TRUE):max(modal_age, na.rm = TRUE))
modal_age_range_ex <-  with(ad_long_ex, min(modal_age, na.rm = TRUE):max(modal_age, na.rm = TRUE))

# set strata to NULL if all are NA
if (all(is.na(ad_long_all[["strata"]]))) config$strata <- NULL

# Overview of samples and readers ##############################################

# Sample overview
sample_data_overview <- sample_data_overview_table(ad_long_all, config$strata)
write.taf(sample_data_overview, dir = "model")

# Participants table
reader_data <- reader_data_table(ad_long_all, strata=config$strata)
write.taf(reader_data, dir = "model")


# Results ##############################################

# repeat for all and for experts only

for (group in c("all", "ex")) {
 #group <- "all"
 #group <- "ex"

  # get the appropriate dataset
  ad_long <- get(vname("ad_long"))
  modal_age_range <- get(vname("modal_age_range"))

  ad_long$modal_age <- factor(ad_long$modal_age, levels = modal_age_range)
  ad_long$reader <- factor(ad_long$reader)

  # number read table
  assign(
    vname("num_read_tab"),
    num_read_table(ad_long, by = "reader")
  )
  write.taf(vname("num_read_tab"), dir = "model")

  # Calculate the number of cases with multiple modes when the traditional method is used, the linear weight or the negative exponential weighting.
  assign(
    vname("multimode_cases_tab_traditional"),
    multimode_cases_table_traditional(ad_long)
  )
  write.taf(vname("multimode_cases_tab_traditional"), dir = "model")
  
  assign(
    vname("multimode_cases_tab_linear"),
    multimode_cases_table_linear(ad_long)
  )
  write.taf(vname("multimode_cases_tab_linear"), dir = "model")
  
  assign(
    vname("multimode_cases_tab_negexp"),
    multimode_cases_table_negexp(ad_long)
  )
  write.taf(vname("multimode_cases_tab_negexp"), dir = "model")

  assign(
    vname("multimode_cases_tab_multistage"),
    multimode_cases_table_multistage(ad_long)
  )
  write.taf(vname("multimode_cases_tab_multistage"), dir = "model")
  

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
      if (nrow(ad_long)) {
        x <- bias_test(ad_long)
      } else {
        data.frame(Comparison = character(0))
      }
    }
  )
  write.taf(vname("bias_tab"), dir = "model")

  
  
  
  
  # Results within strata by reader  #################################################
  if (!is.null(config$strata))
  {
    for (stratum in unique(ad_long$strata)) {
      
      ad_long_strata=ad_long[ad_long$strata==stratum,]
      # number read table
      assign(
        vsname("num_read_tab"),
        num_read_table(ad_long_strata, by = "reader")
      )
      write.taf(vsname("num_read_tab"), dir = "model")
      
      # Calculate the number of cases with multiple modes when the traditional method is used, the linear weight or the negative exponential weighting.
      assign(
        vsname("multimode_cases_tab_traditional"),
        multimode_cases_table_traditional(ad_long_strata)
      )
      write.taf(vsname("multimode_cases_tab_traditional"), dir = "model")
      
      assign(
        vsname("multimode_cases_tab_linear"),
        multimode_cases_table_linear(ad_long_strata)
      )
      write.taf(vsname("multimode_cases_tab_linear"), dir = "model")
      
      assign(
        vsname("multimode_cases_tab_negexp"),
        multimode_cases_table_negexp(ad_long_strata)
      )
      write.taf(vsname("multimode_cases_tab_negexp"), dir = "model")
      
      assign(
        vsname("multimode_cases_tab_multistage"),
        multimode_cases_table_multistage(ad_long_strata)
      )
      write.taf(vsname("multimode_cases_tab_multistage"), dir = "model")
      
      
      # CV table
      assign(
        vsname("cv_tab"),
        cv_table(ad_long_strata, by = "reader")
      )
      write.taf(vsname("cv_tab"), dir = "model")
      
      # APE table
      assign(
        vsname("ape_tab"),
        ape_table(ad_long_strata, by = "reader")
      )
      write.taf(vsname("ape_tab"), dir = "model")
      
      # Percent agreement between age readings and modal age.
      assign(
        vsname("pa_tab"),
        pa_table(ad_long_strata, by = "reader")
      )
      write.taf(vsname("pa_tab"), dir = "model")
      
      
      # Relative bias
      assign(
        vsname("rel_bias_tab"),
        rel_bias_table(ad_long_strata, by = "reader")
      )
      write.taf(vsname("rel_bias_tab"), dir = "model")
      
      # Inter reader bias test - TODO clean bais_test function
      assign(
        vsname("bias_tab"),
        {
          if (nrow(ad_long_strata)) {
            x <- bias_test(ad_long_strata)
          } else {
            data.frame(Comparison = character(0))
          }
        }
      )
      write.taf(vsname("bias_tab"), dir = "model")
    }
  }
  
  
  # Results comparing strata without reader ##############################################

  # loop over strata - 4 tables per strata
  # stratum = "prep_method"
  if (!is.null(config$strata)){

    # Calculate number of readings per reader grouped by modal age and add total
    assign(vname("num_read_tab_by_strata"),
      num_read_table(ad_long, by = "strata")
    )
    write.taf(vname("num_read_tab_by_strata"), dir = "model")

    # CV table
    assign(vname("cv_tab_by_strata"),
      cv_table(ad_long, by = "strata")
    )
    write.taf(vname("cv_tab_by_strata"), dir = "model")
    
    # APE table
    assign(
      vname("ape_tab_by_strata"),
      ape_table(ad_long, by = "strata")
    )
    write.taf(vname("ape_tab_by_strata"), dir = "model")
    
    # Percent agreement between age readings and modal age.
    assign(vname("pa_tab_by_strata"),
      pa_table(ad_long, by = "strata")
    )
    write.taf(vname("pa_tab_by_strata"), dir = "model")

    # Relative bias
    assign(vname("rel_bias_tab_by_strata"),
      rel_bias_table(ad_long, by = "strata")
    )
    write.taf(vname("rel_bias_tab_by_strata"), dir = "model")

  }

  ## Annex tables  #########################################################################

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
  if(group=="ex"){
  
    # Age error matrix (AEM) only for advanced readers, all data together
    assign(
      vname("ae_mat"),
      age_er_matrix(ad_long)
    )
    # todo - do a better job of this...
    saveRDS(get(vname("ae_mat")),
            file = file.path("model", paste0(vname("ae_mat"), ".rds")))
    
    # Age error matrix (AEM) only for advanced readers, by area
    assign(
      vname("ae_mat_by_area"),
      age_er_matrix(ad_long, by = "ices_area")
    )
    # todo - do a better job of this...
    saveRDS(get(vname("ae_mat_by_area")),
            file = file.path("model", paste0(vname("ae_mat_by_area"), ".rds")))
    
    # Age error matrix (AEM) only for advanced readers, by strata
    assign(
      vname("ae_mat_by_strata"),
      age_er_matrix(ad_long, by = config$strata)
    )
    # todo - do a better job of this...
    saveRDS(get(vname("ae_mat_by_strata")),
            file = file.path("model", paste0(vname("ae_mat_by_strata"), ".rds")))
  }
  
}

