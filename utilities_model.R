
##     Exchange analysis for SmartDots
##     Version 1.0 2017
##     get_functions.R


# required libraries
library(plyr)
library(dplyr)
library(data.table)
library(tidyr)
library(scales)
library(tibble)


# Defining functions for use in the analysis ##################################

# TEXT EXPLAINING THE SCRIPT AND THE ANALYSIS.....

###############################################################################

# Overview of functions #######################################################

# reader_info     : returning reader information
# sample_ov       : sample overview
# make_data1      : calculates modal age and cv per sample, long format
# make_data2      : transpose data to wide format with one row per sample,
#                   age readings are now individual columns
# data_ov         : creates Table 1 from GE
# number_readings : Number of age readings per reader per modal age
# get_cv          : Measure of relative variability in the age readings
# get_ape         : Measure of age reading accuracy
# mean_ages       : Mean age per reader by modal age and mean per modal age
# std_age         : stdev for each reader and modal age and total per modal
# get_perc_agree  : Percent of age readings equal to modal age
# get_rel_bias    : Measure of difference between mean age and modal age
# get_overall_rank: Combine ranking values from CV, PA and bias,
#                   and calculate overall ranking
# mean_len_age    : Mean fish lengths per reader and age
# bias_test       : Calculate bias between each pair of readers and between
#                   each reader and modal age
# age_er_matrix   : Relative contributions of ages for each modal age
# style_table0-3  : help function, 4 functions that change the style of output
# number_stata    : number of otolith per strata/month
# cv_strata       : cv per strata/month
# pa_strata       : percentage agreement per strata/month
# rb_strata       : relative buas per strata/month
# rel_dist        : Relative distribution of each age to the modal age
# get_wm          : help function, weighted means per reader or strata
# get_rank        : help function, ranks of each reader based on weighted means
# get_ages        : help function, select only ages in wide format
# setup_nice-2    : help function, Order data nicely from 0:n

# ... and various small functions.



# Reader information ##########################################################

# The function takes the input data (ad_long) and outputs a table of
# reader information on all participating readers
reader_info <- function(data){

  # Keep unique readers
  reader_data <- unique(data[,c("reader", "institute",
                                "country", "expertise")])

  reader_data_out <- reader_data[with(reader_data, order(reader)), ]

  row.names(reader_data_out) <- 1:nrow(reader_data_out)
  return(reader_data_out)
}


# Sample overview #############################################################

# Function to create an overview of number of samples and length ranges
# of the samples grouped by area, year and quarter
sample_ov <- function(dat_in){

  # Get min and max lengths and count number of samples
  ov1 <- ddply(dat_in, .(ices_area, year, qtr), summarise,
               min_len = 5*round(min(length, na.rm = TRUE)/5),
               max_len = 5*round(max(length, na.rm = TRUE)/5),
               num = sum(!is.na(sample)))

  # Determine length range
  ov1$range <- ifelse(ov1$min_len != ov1$max_len,
                      paste0(ov1$min_len,"-",ov1$max_len, " mm"),
                      paste0(ov1$max_len, " mm"))

  # Selecting and renaming variables for output
  ov2 <- ov1[, c("ices_area", "year", "qtr", "range", "num")]
  colnames(ov2) <- c("ICES area", "Year", "Quarter",
                     "Length range", "Number of samples")

  return(ov2)
}



# Table 1 #####################################################################

# Creates Table 1 in GE with sample info, age readings, modal age, CV and PA
# Additionally APE is also added to the output (not part of GE table 1)
#  data_ov(ad_wide, ad_long)
#  dat_in <- ad_wide; dat_in2 <- ad_long
data_ov <- function(dat_in, dat_in2, event_id, report_token) {

  # Select only columns of age readings
  readings <- get_ages(dat_in)

  # Calculate percentage agreement
  dat_in$perc_agree <- round2(rowSums(readings[, -1] == readings$modal_age,
                                      na.rm = TRUE)/
                                      rowSums(!is.na(readings[, -1]))*100)

  # Combine data in correct order

  sample_vars <- select(dat_in, c(FishID, sample, length, sex, catch_date, ices_area))

  dat_out <- as.data.frame(cbind(sample_vars,
                                 select(readings, -modal_age),
                                 select(dat_in, c(modal_age, perc_agree, cv))))
  #Add APE
  ape_tab <- get_ape(dat_in2)
  dat_out2 <- merge(dat_out, ape_tab)

  # Summary information on the age readings
  dat_sum <- transpose(data.frame(colSums(!is.na(readings[-1]), na.rm = T),
                                  colSums(is.na(readings[-1]), na.rm = T) ))
  colnames(dat_sum) <- colnames(readings[-1])
  dat_sum_out <- cbind("Reader" = c("Total number read",
                                    "Total number NOT read"), dat_sum)

  # Summary information on the CV and PA
  sum_stat <- data.frame("Mean PA %" = mean(dat_out2$perc_agree, na.rm = TRUE),
                         "Mean CV %" = mean(dat_out2$cv, na.rm = TRUE),
                         "Mean APE %" = mean(dat_out2$ape, na.rm = TRUE))
  colnames(sum_stat) <- c("Mean PA %", "Mean CV %", "Mean APE %")

  # Renaming selected columns
  date_vals <- "catch_date"
  date_name <- "Catch date"

  names(dat_out2)[names(dat_out2) %in%
                   c("sample", "length", "sex", date_vals, "ices_area",
                     "modal_age", "perc_agree",'cv', 'ape')] <-
                   c("Sample", "Length (mm)", "Sex", date_name,"ICES area",
                     "Modal age", "PA %", "CV %", "APE %")

  # form hyper link for tables
  link_template <-
    sprintf("[%s](http://smartdots.ices.dk/viewImage?tblEventID=%i&SmartImageID=%s&token=%s)",
            "%1$i", event_id, "%1$i", report_token)
  samples <- strsplit(paste(dat_out2$Sample), "-")
  dat_out2$Sample <-
    sapply(samples,
           function(i)
             paste(
               sprintf(link_template, as.numeric(i)),
               collapse = "-")
    )

  return(list(dat_out2, sum_stat, dat_sum_out))

}


# number of age readings ######################################################

# Number of age readings per reader per modal age
number_readings <- function(dat_in) {

  # Remove rows with no age readings
  dat_un <- dat_in[!is.na(dat_in$age), ]

  # Calculate number of readings per reader grouped by modal age and add total
  # number of readings per modal age
  max <- max(dat_un$modal_age, na.rm = TRUE)
  num_read <- as.data.frame.matrix(table(dat_un$modal_age, dat_un$reader)) %>%
              setup_nice(.,max) %>% mutate(total = rowSums(.[, -1]))

  # Add total number of readings per reader
  num_read_out <- rbind(as.data.frame(num_read),
                        c("Total", colSums(num_read[, -1], na.rm = TRUE)))

  # Renaming
  names(num_read_out)[names(num_read_out) %in% "modal_age"] <- "Modal age"

  # output 1: table for used in other calculations/functions
  # output 2: complete table for outputting
  return(list(select(num_read, -modal_age) %>%
                lapply(., as.numeric) %>% setDT(), num_read_out))

}


# Coefficient of Variation (CV) ###############################################

# The CV is a measure of relative variability in the age readings and the
# function outputs a table od CV's per reader and modal age.
# The CV is calculated as the ratio between the mean
# and standard deviation of the ages.
# Total values for all readers and all modal ages are also calculated.
# Readers are ranked from lowest to highest cv.
# n_read <- num_read; dat_inad_wide
get_cv <- function(mean_dat, std_dat, n_read, dat_in){

  # CV, (ratio std/mean)
  cv <- cbind(modal_age = std_dat$modal_age,
              0.5*std_dat[, -c("modal_age")]/
                mean_dat[, -c("modal_age", "all")]*100)
  cv[cv$modal_age==0,] <- NA
  cv$modal_age <- NULL

  # Add overall CV from all individual ages
  all_cv <- dat_in %>% dplyr::group_by(modal_age) %>%
            dplyr::summarize(all = meanNA(as.numeric(cv)) ) %>%
            setup_nice2(., "modal_age")
  all_cv[all_cv$modal_age==0,"all"] <- NA
  cv$all <-  all_cv$all


  cv0 <- as.data.frame(cv)
  cv0[is.nan(cv0)] <- NA

  cv0[] <- Map(paste3, round2(cv0),"%",sep=" ")

  # Add weighted mean
  wm <- get_wm(cv, dat_in, n_read, "y")[[2]]
  names(wm) <- names(cv0)
  cv2 <- rbind(cv0, wm)

  cv2[cv2 == "%"] <- NA
  cv2[cv2 == "NaN %"] <- NA

  # Add modal age and rank
  max <- max(dat_in$modal_age, na.rm = TRUE)
  cv_out <- cbind("modal_age" = c(c(0:max), "Weighted Mean"), cv2)

  names(cv_out)[names(cv_out) %in% "modal_age"] <- "Modal age"

  return(list(cv_out,cbind("modal_age" = 0:max, "cv" = cv$all)))
}


# Average percentage error ####################################################

# The APE is a measure of preduction accuracy that for each sample considers
# the difference between each age reading and the mean age of the sample
get_ape <- function(dat_in) {

  # Calculate APE per sample
  ape <- dat_in %>% filter(modal_age != 0) %>%
         ddply(., .(sample),
               function(x) data.frame( ape = (100/sum(!is.na(x$age)))*
                           sumNA(abs(x$age - meanNA(x$age))/meanNA(x$age))))

  # Rounding
  ape$ape <- round2(as.numeric(ape$ape))

  return(ape)

}

# Mean age calculations #######################################################

# Mean age per reader per modal age and average mean age per modal age.
mean_ages <- function(dat_in){

  # Mean age per reader per modal age
  mean_ages <- as.data.table(dat_in)[, lapply(.SD, meanNA), by = modal_age]
  mean_ages[is.nan(mean_ages)] <- NA

  # Reorganize and add overall mean column
  ma_out <- mean_ages %>% setup_nice2(., "modal_age") %>%
            mutate(all = rowMeans(.[, -1], na.rm = TRUE))

  ma_out[is.nan(ma_out)] <- NA
  ma_out %>% as.data.table
}



# standard deviation of age readings ##########################################

# 2 times standard deviation per reader per modal age
std_ages <- function(dat_in){

  # calculates 2 stdev per modal age and reader
  std_reader <- as.data.table(dat_in)[, lapply(.SD,
                        function(x) (sdNA(x)*2)), by = modal_age] %>%
                setup_nice2(.,"modal_age")

  # sd and 2sd per modal age
  std_all <- dat_in %>%
             gather("reader", "value", -modal_age) %>%
             group_by(modal_age) %>%
             summarise(sd2 = 2*sd(value, na.rm = TRUE),
                       sd = sd(value, na.rm = TRUE)) %>%
             setup_nice2(., "modal_age")

  # If only one occurence of modal age, then sd=0 --- OBS!!!
  std_reader[is.na(std_reader) &
             std_reader$modal_age %in% dat_in$modal_age] <- 0

  return(list(std_reader, std_all))

}


# Percentage agreement ########################################################

# Get % agreement (rate of number of age reading = modal age) per reader
# by modal age and total per modal age. Rank is assigned based on a
# weighted mean (based on number of readings) per reader.
# get_perc_agree(ad_long, num_read)
# dat_in <- ad_long; n_read <- num_read
get_perc_agree <- function(dat_in, n_read){

  # Get data with agreement between reading and modal age and
  # create table with number of agreements per reader
  n_agree <- as.data.table(dat_in)[age == modal_age,]
  max_age <- max(dat_in$modal_age, na.rm = TRUE)

  n_agree2 <-
    table(n_agree$modal_age, factor(n_agree$reader, levels = sort(unique(dat_in$reader)))) %>%
    as.data.frame.matrix %>%
    setup_nice(., max_age) %>%
    select(-modal_age)
  n_agree2 <- n_agree2 / select(as.data.table(n_read), -total) * 100

  # overall agreement per modal age
  n_agree2$all <-
    as.data.table(table(n_agree$modal_age)) %>%
    setup_nice(., max_age) %>%
    dplyr::select(-modal_age) / n_read$total * 100
    n_agree2[is.nan(n_agree2)] <- NA

  # Add % sign to cells
  new_PA <- as.data.frame(n_agree2)
  new_PA[] <- sprintf("%0.0f %%", round2(unlist(new_PA)))

  # Combined agreement table
  pa <-  cbind("modal_age" = c(0:max_age, "Weighted Mean"),
        rbind(new_PA, get_wm(n_agree2, n_read)[[2]] ) )

  # Minor cleaning up..
  pa[pa=="NA %"] <- NA
  names(pa)[names(pa) %in% c("modal_age")] <- c("Modal age")

  list(pa, cbind("modal_age" = 0:max_age, "pa" = n_agree2$all))
}



# Relative bias table #########################################################

#data_modal

# Relative bias: difference between mean age and modal age per reader
# and for all readers combined. Readers are ranked based on a weighted mean
# of the biases per reader.
# get_rel_bias(mean_dat,num_read)
# n_read <- num_read
get_rel_bias <- function(mean_dat, n_read){

  # calculate difference per reader: mean age - modal age
  max <- max(mean_dat$modal_age, na.rm = TRUE)
  rel <- round(mean_dat[, -1] - 0:max, 2)
  rel_n <- rel
  rel <- as.data.frame(rel)

  # numebrs to string to keep two digits
  rel[] <- sprintf("%0.2f %%", unlist(rel) * 100)
  rel <- cbind(modal_age = 0:max, rel)

  # Combine with weighted mean and rank
  wm <- get_wm(rel_n, n_read, "n", FALSE)
  wm[] <- sprintf("%0.2f %%", unlist(wm) * 100)

  # Combine
  rel_out <- rbind(rel,
                   c("Weighted Mean", wm))

  # For outputting only total bias per modal age
  rel_num <- as.data.frame(cbind("modal_age" = 0:max,
                                 "all" = rel_n$all))

  # Minor cleaning
  rel_out[rel_out=="NA %"] <- NA
  names(rel_out)[names(rel_out) %in% "modal_age"] <- "Modal age"

  list(rel_out, rel_num)
}


# overall_ranking #############################################################

# Combine ranking from CV, PA and relative bias tables
# Overall rank is found based on ranking values in the three tables
# rank_tab <- get_overall_rank(rbind(cv_tab, pa_tab, rb_tab))
# dat_in <- rbind(cv_tab, pa_tab, rb_tab)
get_overall_rank <- function(dat_in){

  # Renaming and get rows with ranking values and only reader columns
  names(dat_in)[1] <- "modal_age"
  ranking <- dat_in %>%
             filter(modal_age == "Rank")  %>%
             dplyr::select(-all, -modal_age)

  # Caculate total ranking and create output
  total_ranking <- rank(colMeans( apply(ranking, 2, as.numeric)),
                        ties.method = "min")

  cbind(Ranking = c("Coefficient of Variation",
                                     "Percentage agreement",
                                     "Relative bias",
                                     "Total"),
        rbind(ranking, total_ranking))
}


# Age composition #############################################################

# Summary of number of otoliths that each reader have read
age_compo <- function(dat_in){

  # Number of otoliths read per reader and age
  num_oto <- as.data.frame.matrix(unlist(table(dat_in$age, dat_in$reader))) %>%
             setup_nice1(.,"age",max(dat_in$age, na.rm=T)) %>%
             mutate(all = rowSums(.[, -1])) %>% as.data.frame()

  # Add total numbers per reader
  num_o_out <- rbind(num_oto, c("Total", colSums(num_oto[, -1], na.rm = TRUE)))

  return(num_o_out)

}


# Mean Length at Age ##########################################################

mean_len_age <- function(dat_in, ac, max_age){

  # Fish mean length per age and reader
  mean_len_num  <- dat_in %>% group_by(age, reader) %>%
                   dplyr::summarize(mean_len = meanNA(length) ) %>%
                   as.data.frame()

  # Long to wide format and calc mean overall lengths per age
  mean_len <- spread(mean_len_num, reader, mean_len) %>% setDT %>%
              mutate(total = rowMeans(.[, -1], na.rm = TRUE)) %>%
              setup_nice2(.,"age") %>% as.data.frame() %>% select(-age)

  # Adding units..
  new_ml <- as.data.frame(lapply(mean_len, function(x) paste(round2(x), "mm")),
                          stringsAsFactors = FALSE)
  names(new_ml) = gsub(pattern = "\\.", replacement = " ", names(new_ml))

  # Calculate weighted mean length per reader
  ac_num <-  lapply(ac[-nrow(ac), -1], as.numeric) %>% as.data.frame()
  wm <- as.data.frame(lapply(colSums(mean_len*ac_num, na.rm = TRUE)/
                                colSums(ac_num, na.rm = TRUE) ,
                             function(x) paste(round2(x), "mm")),
                      stringsAsFactors = FALSE
                      )

  # Re-naming and combine mean lengths per reader and age with weighted means
  names(wm) = gsub(pattern = "\\.", replacement = " ", names(wm))
  mean_length <- cbind("age" = c(0:max_age, "Weighted Mean"),
                       rbind(new_ml, wm ))
  mean_length[mean_length == "NA mm"] <- NA

  return(list(mean_length, mean_len_num))

}

# Inter Reader Bias Test ######################################################

# This test compares the bias pairwise between readers and between readers
# and modal age.
# The bias test is made according to the analysis in the GE sheet.
# We have not been able to find the theory behind this method
# and all equaitons and calculations are therfore taken directly from GE..

bias_test <- function(dat_in){

  dat_un <- dat_in

  # For each pair of readers (and each reader compared to th modal age of
  # the sample) and each image get reading difference
  diffs <- outer(1:ncol(dat_un), 1:ncol(dat_un),
                 function(x, y) dat_un[, y] - dat_un[, x])
  colnames(diffs) <- outer(colnames(dat_un),
                           colnames(dat_un), paste, sep = ":")

  #Prepare inter reader bias table
  int_bias <- data.frame(matrix(NA, nrow = length(dat_un),
                                 ncol = length(dat_un) -1))
  colnames(int_bias) <- colnames(dat_un[, -1])
  rownames(int_bias) <- c(colnames(dat_un[, -1]), "modal_age")

  #Loop through each pair
  for (i in 1:ncol(diffs)) {

    # Frequency table of the differnces between each reader combination
    data <- data.frame(table(diffs[i]))
    data$Var1 <- as.numeric(as.character(data$Var1)) #Difference
    data$Freq <- as.numeric(as.character(data$Freq)) #Frequency of difference
    data2 <- data[data$Var1!=0,]

    # Loop through differences and calculate ranking values according to Guus Eltink
    # done for both neagtive and positive differences simultaneously
    if ( nrow(data2) > 0 ){
      for (j in 1:nrow(data2)) {
        data2$times[j] <- data2$Freq[j]*
                          (0.5*sum(data2[abs(data2$Var1) ==
                                         abs(data2$Var1[j]), 2] + 0.5) +
                           sum(data2[abs(data2$Var1) <
                                       abs(data2$Var1[j]), 2]))
      }

      pos_val <- sum(data2[data2$Var1 > 0, ]$times) #Guus Eltink: "R+"
      neg_val <- sum(data2[data2$Var1 < 0, ]$times) #Guus Eltink: "R-"
      valsN <- sum(data[data$Var1 != 0, 2]) #no. of oberservations with difference

      # Naming: reader1:reader2
      name1 <- gsub( ":.*$", "", colnames(diffs[i]))
      name2 <- sub('.*\\:', '', colnames(diffs[i]))

      # Calcualte result of comparison
      result <- (min(pos_val, neg_val)-(valsN*(valsN + 1))/4)/
                                  (sqrt((valsN*(valsN + 1)*(2*valsN + 1))/24))

      #Add result to bias table - tjek both possible combinations
      int_bias[rownames(int_bias) == name2,
                colnames(int_bias) == name1] <- result
      int_bias[rownames(int_bias) == name1,
                colnames(int_bias) == name2] <- result
     }

     # Depending on bias value (z in Guus Eltink) assign symbols:
     int_bias2 <- abs(as.matrix(int_bias))
     int_bias2[int_bias2 < 1.96 | is.nan(int_bias2)] <- "-"
     int_bias2[int_bias2 > 2.58 ] <- "**"
     int_bias2[int_bias2 < 2.58 & int_bias2 > 1.96 ] <- "*"

  }

  # Last corrections..
  #int_bias2[is.na(int_bias2)] <- ""
  row.names(int_bias2)[nrow(int_bias2)] <- "Modal age"
  int_bias3 <- as.data.frame(int_bias2)
  int_bias3 <- rownames_to_column(int_bias3, var="Comparison")

  return(int_bias3)
}

# AGE ERROR MATRIX  ###########################################################

# The AEM calculates the relative contribution of each age to the modal age.
# The AEM is calculated per area and only including the readings
# of the advanced readers.

age_er_matrix <- function(dat_in){

  # Pre-set list to add AEMs for each ices area
  list_aem <-list()

  areas <- unique(dat_in$ices_area)
  for (i in areas) {

  # Relative contribution of each age per modal age (long format)
  calc_rel_age <- ddply(dat_in[complete.cases(dat_in$age) &
                                 dat_in$ices_area==i,],
                        .(modal_age, age), summarize,
                        age_per_modal = sum(!is.na(age)))   %>%
                  merge(., ddply(., .(modal_age), summarize,
                        tot_per_modal = sum(age_per_modal) ) ) %>%
                  mutate(rel_age = age_per_modal/tot_per_modal)

  # Matrix on relative contributions of each age per modal age (wide format)
  ages_err1 <-  spread(calc_rel_age[c("age", "modal_age", "rel_age")],
                       age, rel_age) %>% setup_nice2(., "modal_age-age")

  # Transpose matrix such that each column sum to 1
  ages_trans <- setNames(as.data.frame(t(ages_err1[, -1])),
                         unlist(ages_err1[, 1])) %>%
                cbind("modal_age" = row.names(.), .)


  # Last corrections and add data frame to list
  ages_trans$modal_age <- as.integer(as.character(ages_trans$modal_age))
  ages_trans2 <- setup_nice2(ages_trans, "modal_age-age") %>%
                 mutate(modal_age = paste("Age", modal_age))
  names(ages_trans2)[names(ages_trans2) %in% c("modal_age")] <- c("Modal age")
  list_aem[[i]] <- ages_trans2

  }

  return(list(list_aem, areas))

}


age_er_matrix_stock <- function(dat_in){

  # Pre-set list to add AEMs for each ices area
  list_aem <-list()

  stocks <- unique(dat_in$stock)
  for (i in stocks) {

  # Relative contribution of each age per modal age (long format)
  calc_rel_age <- ddply(dat_in[complete.cases(dat_in$age) &
                                 dat_in$stock==i,],
                        .(modal_age, age), summarize,
                        age_per_modal = sum(!is.na(age)))   %>%
                  merge(., ddply(., .(modal_age), summarize,
                        tot_per_modal = sum(age_per_modal) ) ) %>%
                  mutate(rel_age = age_per_modal/tot_per_modal)

  # Matrix on relative contributions of each age per modal age (wide format)
  ages_err1 <-  spread(calc_rel_age[c("age", "modal_age", "rel_age")],
                       age, rel_age) %>% setup_nice2(., "modal_age-age")

  # Transpose matrix such that each column sum to 1
  ages_trans <- setNames(as.data.frame(t(ages_err1[, -1])),
                         unlist(ages_err1[, 1])) %>%
                cbind("modal_age" = row.names(.), .)


  # Last corrections and add data frame to list
  ages_trans$modal_age <- as.integer(as.character(ages_trans$modal_age))
  ages_trans2 <- setup_nice2(ages_trans, "modal_age-age") %>%
                 mutate(modal_age = paste("Age", modal_age))
  names(ages_trans2)[names(ages_trans2) %in% c("modal_age")] <- c("Modal age")
  list_aem[[i]] <- ages_trans2

  }

  return(list(list_aem, areas))

}



# number of modal ages per strata/month #######################################

# Empty data frame with months
df_month <- cbind(data.frame(matrix(ncol=1,nrow=0)),
                  data.frame(matrix(ncol=12,nrow=0)))
colnames(df_month) <- c("modal_age", month.abb[c(1:12)])



# Number of samples per month/strata and modal age

number_strata <- function(dat_in, var){

  # Select variables and re-name months
  dat_un <- dat_in[,c("modal_age", var, "sample")]
  if (var == "month") {
    dat_un <- dat_un %>% mutate(month = month.abb[month])
  }

  # Calculate samples per strata by modal age
  max <- max(dat_un$modal_age, na.rm=T)
  num_mon <- as.data.frame.matrix(table(dat_un[, "modal_age"],
                                        dat_un[, var])) %>%
             setup_nice(., max) %>% mutate(Total = rowSums(.[, -1]))

  # Order columns. If month order months correctly else alphabetically
  if (var == "month" ) {
    num_mon2 <- merge(df_month, num_mon, all = TRUE)[, union(names(df_month),
                                                             names(num_mon))]
  } else {
    num_mon2 <- num_mon[, -1 & order(names(num_mon))]
  }

  # Final corrections and output
  names(num_mon2)[names(num_mon2) %in% c("modal_age")] <- c("Modal age")
  return(list(num_mon2[, -1],
              rbind(num_mon2,
                    c("Total", colSums(num_mon2[, -1], na.rm = TRUE)),
                    c("Total %", percent(colSums(num_mon2[, -1],
                                      na.rm = TRUE)/nrow(dat_un) ) ))))

}


# cv of modal age per month/strata ############################################

# Calculate CV per month, area, stock...
# cv_strata(ad_wide, num_st, config$strata)
# dat_in <- ad_wide; num <- num_st; var <- config$strata
cv_strata <- function(dat_in, num, var){

  # Select data and re-name months
  dat_un <- dat_in[, c("modal_age", var, "sample", "cv")]
  if (var == "month") {
    dat_un <- dat_un %>% mutate(month = month.abb[month])
  }

  # CV per monh by modal age
  mon_cv <- ddply(dat_un, c("modal_age", var), dplyr::summarize,
                  all = meanNA(as.numeric(cv)))  %>%
                  spread_(., var, "all")

  # CV for all months combined
  all_cv <- cbind(mon_cv, "all" = ddply(dat_un, .(modal_age), dplyr::summarize,
                         all = meanNA(as.numeric(cv)))[[2]] ) %>%
            setup_nice2(., "modal_age") %>% as.data.frame()

  # Order columns. If month order months correctly else alphabetically
  if (var == "month" ) {
    num_mon2 <- merge(df_month, all_cv, all =TRUE)[, union(names(df_month),
                                                           names(all_cv))]
  } else {
    num_mon2 <- all_cv[ , -1 & order(names(all_cv))]
  }

  # Weighted mean per month/strata and corrections
  wm <- paste(round2(colSums(num_mon2[,-1]*num, na.rm = TRUE)/
                       colSums(num, na.rm = TRUE)), "%")
  num_mon3 <- round2(num_mon2[,-1])
  num_mon3[] <- paste(unlist(num_mon3), "%")

  # Combine all
  num_mon_out <- cbind("Modal age" = c(c(0:max(dat_un$modal_age, na.rm=T)),
                                       "Weighted Mean"), rbind(num_mon3, wm))
  num_mon_out[num_mon_out == "NaN %" | num_mon_out == "NA %"] <- NA

  return(num_mon_out)
}


# Percentage agreement per month/strata #######################################

# Percentage agreement per strata/month
# pa_strata(ad_long, num_mo, "month")
# n_read <- num_mo; var <- "month"; dat_in <- ad_long
pa_strata <- function(dat_in, n_read, var){

  # Re-naming month
  if (var=="month") {
    dat_un <- dat_in %>% mutate(month = month.abb[month])
  } else {
    dat_un <- dat_in
  }

  # Number of agreements per modal age and strata
  n_agree <- dat_un[dat_un$age == dat_un$modal_age,] %>% as.data.frame
  max <- max(dat_un$modal_age, na.rm = TRUE)
  n_agree2 <- as.data.frame.matrix(unlist(table(
                      n_agree[, "modal_age"], n_agree[, var]))) %>%
              setup_nice(.,max)

  # Number of samples per modal age and strata
  dat_un <- as.data.frame(dat_un)
  all <- as.data.frame.matrix(
                 unlist(table(dat_un[, "modal_age"], dat_un[, var]))) %>%
         setup_nice(.,max)

  # Get percentage agreement
  n_agree3 <- cbind(modal_age = n_agree2$modal_age, n_agree2[,-1]/all[,-1]*100)

  # Order columns. If month order months correctly else alphabetically
  n_agree3 <- as.data.frame(n_agree3)
  if (var == "month" ) {
    agree_mon <- merge(df_month, n_agree3, all =TRUE)[, union(names(df_month),
                                                              names(n_agree3))]
  } else {
    agree_mon <- n_agree3[ , -1 & order(names(n_agree3))]
  }

  # Total for all months
  all_pa <-  as.data.table(table(n_agree$modal_age)) %>%
             setup_nice(.,max) %>%
             select(-modal_age)/rowSums(all[, -1], na.rm = TRUE)*100
  agree_mon$all <- all_pa$N

  # Weighted mean
  wm <- paste(round2(colSums(agree_mon[, -1]*n_read, na.rm = TRUE)/
                       colSums(n_read, na.rm = TRUE)), "%")
  # Add percentage sign to results
  agree_mon2 <- round2(agree_mon[,-1])
  agree_mon2[] <- paste(unlist(agree_mon2), "%")

  # Combined agreement table
  agree_mon_out <- cbind("Modal age" = c(c(0:max), "Weighted Mean"),
                       rbind(agree_mon2, wm))
  agree_mon_out[agree_mon_out == "NA %" | agree_mon_out == "NaN %"] <- NA

  agree_mon_out
 }


# Relative bias per month/strata ##############################################

# Relative bias per month or strata

rb_strata <- function(dat_in, n_read, var){

  # Rename month
  if (var == "month") {
    dat_un <- dat_in %>% mutate(month = month.abb[month])
  } else {
    dat_un <- dat_in
  }

  # Mean age and bias per modal age
  mean_mo <- ddply(dat_un, c("modal_age", var), dplyr::summarize,
                   age_mean = meanNA(age) )
  mean_mo$bias <- round(mean_mo$age_mean - mean_mo$modal_age, 2)

  # long to wide format. Bias per modal age and strata
  rb <- as.data.frame(mean_mo) %>% select(-age_mean) %>%
        spread_(., var, "bias") %>% setDT %>%
         setup_nice2(., "modal_age") %>% as.data.frame()

  # Order columns. If month order months correctly else alphabetically
  if (var == "month" ) {
    rb_mon <- merge(df_month, rb, all = TRUE)[, union(names(df_month),
                                                     names(rb))]
  } else {
    rb_mon <- rb[ , -1 & order(names(rb))]
  }

  # Calcualte overall mean bias per modal age
  mb <- ddply(dat_un, c("modal_age"), dplyr::summarize,
              mean_bias = meanNA(age) - sort(unique(modal_age))) %>%
        setDT %>% setup_nice2(., "modal_age")
  rb_mon$mean_bias <- mb$mean_bias


  # Weighted mean
  wm <- paste(round(colSums(rb_mon[, -1]*n_read, na.rm = TRUE)/
                colSums(n_read, na.rm = TRUE), 2), "%")

  # Add percentage sign to results
  rb_mon2 <- round2(rb_mon[,-1])
  rb_mon2[] <- paste(unlist(rb_mon2), "%")

  # Combined agreement table
  rel_out <- cbind("Modal age" = c(c(0:max(rb_mon$modal_age, na.rm = TRUE)), "Weighted Mean"),
                       rbind(rb_mon2, wm))
  rel_out[rel_out == "NA %" | rel_out == "NaN %"] <- NA

  # Corrections
  names(rel_out)[names(rel_out) %in% c("mean_bias")] <- "Mean bias"

  return(rel_out)

}




# Table of differences per modal ages #########################################

# Relative contributions of each age to the modal age

rel_dist <- function(dat_in, num_r){

  # Get difference in age reading per reader and sample
  dat_in$age_dif <- dat_in$age - dat_in$modal_age

  # Total number of differences grouped by size in difference and modal age
  dr <- dat_in %>% dplyr::group_by(modal_age, age_dif) %>%
        dplyr::summarize(calc_dif = sum(!is.na(age_dif)) ) %>%
        as.data.frame() %>% filter(!is.na(age_dif))

  # Get interval of differences and dummy data of non occuring differences
  dif_int <- -abs(max(dr$age_dif, na.rm=T)):abs(max(dr$age_dif, na.rm = T))
  all_difs <- CJ(modal_age = 0, age_dif = dif_int, calc_dif = 0)
  all_difs2 <- all_difs[!(all_difs$age_dif %in% dr$age_dif),]

  # add dummy data, calc percentage of relative contributions and
  # and make table into wide format
  dr2 <- rbind(dr, all_difs2) %>%
            spread(., age_dif, calc_dif) %>% setDT %>%
            setup_nice2(., "modal_age") %>%
            select(- modal_age)/num_r$total*100

  # Add sum and make final corrections
  dr2$sum <- rowSums(dr2, na.rm = T)
  dr2[is.na(dr2) | dr2 == 0 ] <- 0
  dr_out <- as.data.table(lapply(dr2, function(x) paste0(round2(x), "%")))

  return(list(dr_out, cbind(modal_age = 0:max(dat_in$modal_age, na.rm=T), dr2[, -"sum"])))

}



# Weighted mean ###############################################################

# Weighted mean

# Calculate and output weighted mean of input data weighted on sumber of
# age readings per modal age and reader
# Outputting also a version of wm with added % sign to the results if needed
# get_wm(cv, n_read, "y")
# data <- cv; nr <- n_read; val <- "y"
get_wm <- function(data, nr, val = "n", cor = TRUE) {

  # Calcaulte weighted means
  wm <- colSums(data * nr, na.rm = TRUE) / colSums(nr, na.rm = TRUE)

  if (cor) {
    # Total value
    if (val == "y") {
      wm["all"] <- mean(as.numeric(data$cv), na.rm = TRUE)
    }

    # Adding percentage sign to results
    wm_out <- wm
    wm_out[] <- sprintf("%.1f %%", round(wm, 1))

    return(list(wm, wm_out))
  } else {
    return(wm)
  }
}


# ranking function ############################################################

# Rank readers based on the weighted means
# Ties method is min, so in case of equal weighted means the two readers will
# we ranked equally and both with the lowest (best) rank

get_rank <- function(data, nr, char, cor = TRUE){

  if (cor == TRUE) {
    w_mean <- get_wm(data, nr, "n")[[1]] # Get weighted mean values
    as.character(rank(char*abs(w_mean[-length(w_mean)]), ties.method = "min"))
  } else {
    w_mean <- get_wm(data, nr,"n", FALSE) # Get weighted mean values
    as.character(rank(abs(w_mean[-length(w_mean)]), ties.method = "min"))
  }

}


# Select ages #################################################################

# Small functions that select only age readings and modal ages
# from data in wide format
get_ages <- function(dat_in){

   cols <- which(names(dat_in) == "modal_age"):ncol(dat_in)
   dat_out <- dplyr::select(dat_in, cols)
  return(dat_out)
}


# order data nicely ###########################################################

# Different setups to order data from 0:n either by age or modal age

setup_nice <- function(dat_in, max){

  clean_dat <- setDT(dat_in, keep.rownames = TRUE) %>%
               setnames(., 1, "modal_age") %>%
               mutate(modal_age=as.integer(modal_age)) %>%
               setDT(key = "modal_age") %>%
               .[CJ(0:max),]

}

setup_nice1 <- function(dat_in, key_in,max){

  clean_dat <- setDT(dat_in, keep.rownames = TRUE) %>%
                setnames(., 1, key_in) %>% mutate(age = as.integer(age)) %>%
                setDT(key = key_in) %>%
                .[CJ(0:max),]

}


setup_nice2 <- function(dat_in, key_in){

  if (key_in == "modal_age") {
    max <- max(dat_in$modal_age, na.rm=T)
  } else if (key_in == "age"){
    max <- max(dat_in$age, na.rm=T)
  } else if (key_in == "modal_age-age"){
    max <- as.numeric(colnames(dat_in)[ncol(dat_in)])
    key_in <- "modal_age"
  }

  setDT(dat_in, key = key_in) %>% .[CJ(0:max)]
}


# List function needed to output multiple variables from function
list <- structure(NA, class = "result")

"[<-.result" <- function(x,..., value) {
  args <- as.list(match.call())
  args <- args[ -c(1:2, length(args))]
  length(value) <- length(args)

  for(i in seq(along = args)) {
    a <- args[[i]]
    if(!missing(a)) eval.parent(substitute(a <- v,
                                           list(a = a, v = value[[i]])))
  }
  x
}


