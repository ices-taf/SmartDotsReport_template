## Run analysis, write model results

## Before: data/ad_long.csv, data/ad_long_ex.csv,
##         data/ad_wide.csv, data/ad_wide_ex.csv
## After:

library(icesTAF)
library(jsonlite)

# make model directory
mkdir("model")

# load configuration
config <- read_json("config.json", simplifyVector = TRUE)

# download the report and summary templates
download.file(url = config$report_template, "model/reportTemplate.docx", quiet = TRUE, mode = "wb")
download.file(url = config$summary_template, "model/summaryTemplate.docx", quiet = TRUE, mode = "wb")

# load utilities
source("utilities_model.R")
source("utilities.R")



# read input data
ad_long <- read.taf("data/ad_long.csv")
ad_long_ex <- read.taf("data/ad_long_ex.csv")
ad_wide <- read.taf("data/ad_wide.csv")
ad_wide_ex <- read.taf("data/ad_wide_ex.csv")

# Participants and data overview ##############################################

# Participants
part_tab <- reader_info(ad_long)
part_tab2 <- part_tab

colnames(part_tab) <- c("Reader code", "Institution", "Country", "Expertise")
part_tab <- part_tab[order(part_tab$`Reader code`),]
rownames(part_tab) <- NULL

# Sample overview
sample_dat_ov <- sample_ov(ad_wide)

# Max values need for output tables
max_age <- max(ad_long$age, na.rm = TRUE)
max_modal <- max(ad_long$modal_age, na.rm = TRUE)
max_age_ex <- max(ad_long_ex$age, na.rm = TRUE)
max_modal_ex <- max(ad_long_ex$modal_age, na.rm = TRUE)

# TABLES 6 GE - Mean and standard deviation ####################################

# Tables are needed for calculation of some other tables...

# Mean ages per modal age per reader
mean_dat <- mean_ages(get_ages(ad_wide))
mean_dat_ex <- mean_ages(get_ages(ad_wide_ex))

# Standard deviation per modal age per reader
list[std_dat, std_all] <- std_ages(get_ages(ad_wide))
list[std_dat_ex, std_all_ex] <- std_ages(get_ages(ad_wide_ex))

# TABLES 1 GE - complete data overview ########################################

# Complete data overview including stats per sample
list[table1_data, sum_stat, sum_readings] <- data_ov(ad_wide, ad_long)
list[table1_data_ex, sum_stat_ex, sum_readings_ex] <- data_ov(ad_wide_ex,
                                                              ad_long_ex)

# TABLES 2 GE - modal age statistics per reader ###############################

# Number of age readings
list[num_read, num_read_comp] <- number_readings(ad_long)
list[num_read_ex, num_read_comp_ex] <- number_readings(ad_long_ex)

# Create CV table ### OBS!!
list[cv_tab, cv_all] <- get_cv(mean_dat, std_dat, num_read, ad_wide)
list[cv_tab_ex, cv_all_ex] <- get_cv(mean_dat_ex, std_dat_ex,
                                     num_read_ex, ad_wide_ex)

# Percent agreement between age readings and modal age.
list[pa_tab, pa_all] <- get_perc_agree(ad_long, num_read)
list[pa_tab_ex, pa_all_ex] <- get_perc_agree(ad_long_ex, num_read_ex)

# Relative bias
list[rb_tab, rb_all] <- get_rel_bias(mean_dat,num_read)
list[rb_tab_ex, rb_all_ex] <- get_rel_bias(mean_dat_ex, num_read_ex)

# TABLES 3 GE - age statistics per reader #####################################

# Age composition
ac_tab <- age_compo(ad_long)
ac_tab_ex <- age_compo(ad_long_ex)

# Mean length at age
list[mla_tab,mla_num] <- mean_len_age(ad_long, ac_tab, max_age)
list[mla_tab_ex,mla_num_ex] <- mean_len_age(ad_long_ex, ac_tab_ex, max_age)

# Inter reader bias test
bias_tab <- bias_test(get_ages(ad_wide))
bias_tab_ex <- bias_test(get_ages(ad_wide_ex))


# TABLES 4 GE - Statistics per month ##########################################

# Number readings per month
list[num_mo, num_mo_comp] <- number_strata(ad_wide, "month")
list[num_mo_ex, num_mo_comp_ex] <- number_strata(ad_wide_ex, "month")

# CV per month
cv_mo_tab <- cv_strata(ad_wide, num_mo, "month")
cv_mo_tab_ex <- cv_strata(ad_wide_ex, num_mo_ex, "month")

# PA per month
pa_mo_tab <- pa_strata(ad_long, num_mo, "month")
pa_mo_tab_ex <- pa_strata(ad_long_ex, num_mo_ex, "month")

# Relative bias per month
rb_mo_tab <- rb_strata(ad_long, num_mo, "month")
rb_mo_tab_ex <- rb_strata(ad_long_ex, num_mo_ex, "month")


# Age error matrix ############################################################

# Age error matrix (AEM)
list[ae_mat_ex, areas] <- age_er_matrix(ad_long_ex)

# Plot calculations/info #######################################################

# Sample info needed for growth analysis
compl_sample <- ad_wide[complete.cases(select(get_ages(ad_wide))),"sample"]
compl_sample_ex <- ad_wide_ex[complete.cases(get_ages(ad_wide_ex)), "sample"]

# Relative distribution of ages for each modal age
list[dif_tab_co, dif_tab] <- rel_dist(ad_long, num_read)
list[dif_tab_co_ex, dif_tab_ex] <- rel_dist(ad_long_ex, num_read_ex)

# Age error matrix per stock (AEM)
stocks <- unique(ad_long_ex$stock)
list[ae_mat_stock, areas_stock] <- age_er_matrix_stock(ad_long_ex)


# write (almost) everything out
list_of_obj <-
  c("ac_tab", "ac_tab_ex", "ad_long", "ad_long_ex", "ad_wide", "ad_wide_ex",
    "ae_mat_ex", "areas", "bias_tab", "bias_tab_ex", "compl_sample", "compl_sample_ex",
    "cv_all", "cv_all_ex", "cv_mo_tab", "cv_mo_tab_ex",
    "cv_tab", "cv_tab_ex", "dif_tab", "dif_tab_co", "dif_tab_co_ex", "dif_tab_ex",
    "max_age", "max_age_ex", "max_modal", "max_modal_ex", "mean_dat", "mean_dat_ex",
    "mla_num", "mla_num_ex", "mla_tab", "mla_tab_ex", "num_mo", "num_mo_comp",
    "num_mo_comp_ex", "num_mo_ex", "num_read", "num_read_comp", "num_read_comp_ex",
    "num_read_ex",
    "pa_all", "pa_all_ex", "pa_mo_tab", "pa_mo_tab_ex",
    "pa_tab", "pa_tab_ex", "part_tab", "part_tab2", "rb_all",
    "rb_all_ex", "rb_mo_tab", "rb_mo_tab_ex", "rb_tab",
    "rb_tab_ex", "sample_dat_ov", "std_all", "std_all_ex", "std_dat", "std_dat_ex",
    "stocks", "ae_mat_stock", "areas_stock",
    "sum_readings", "sum_readings_ex", "sum_stat", "sum_stat_ex", "table1_data",
    "table1_data_ex")

for (obj in list_of_obj) {
  x <- get(obj)
  if (inherits(x, "data.frame")) {
    write.taf(x, paste0("model/", obj, ".csv"))
  } else {
    save.call <- sprintf('save(%1$s, file = "model/%1$s.rData")', obj)
    eval(parse(text = save.call))
  }
}

