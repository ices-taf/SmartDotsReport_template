

format_table <- function(tab, fmt = "%i", extra_rows = "Total", age_range = modal_age_range) {
  tab[] <- lapply(tab, function(x) ifelse(is.na(x), "-", sprintf(fmt, x)))
  cbind(`Modal age` = c(age_range, extra_rows), tab)
}


sample_data_overview_table <- function(ad_long, strata) {
  sample_data_overview <-
    ad_long %>%
    select_at(unique(c("FishID", "sample", "length", "modal_age", "year", "ices_area", "qtr", strata, "reader", "age"))) %>%
    spread(key = reader, value = age) %>%
    group_by_at(unique(c("year", "ices_area", "qtr", strata))) %>%
    summarise(
      min_len = 5*round(min(length, na.rm = TRUE)/5),
      max_len = 5*round(max(length, na.rm = TRUE)/5),
      num = sum(!is.na(sample)),
      min_ma = min(modal_age, na.rm = TRUE),
      max_ma = max(modal_age, na.rm = TRUE)
    ) %>%
    ungroup %>%
    # Determine length range
    mutate(
      Quarter = qtr,
      `Number of samples` = num,
      `Modal age range` = ifelse(min_ma != max_ma,
                      paste0(min_ma, "-", max_ma),
                      paste0(max_ma)),
      `Length range` = ifelse(min_len != max_len,
                     paste0(min_len, "-", max_len, " mm"),
                     paste0(max_len, " mm"))
    ) %>%
  select(-min_len, -max_len, -num, -min_ma, -max_ma, -qtr)%>%
  as.data.frame

# rename columns
names(sample_data_overview) <-
  sample_data_overview %>%
  names %>%
  gsub("ices", "ICES", .) %>%
  gsub("_", " ", .) %>%
  capFirst

  sample_data_overview
}






numbers_read <- function(data, by = "reader") {
  table(data$modal_age, data[[by]]) %>%
  unclass %>%
  as.data.frame %>%
  mutate(total = unname(rowSums(.)))
}


num_read_table <- function(ad_long, by = "reader") {
  ad_long %>%
    numbers_read(by = by) %>%
    rbind(colSums(., na.rm = TRUE)) %>%
    format_table
}

cv_table <- function(ad_long, by = "reader") {

  cv_tab <-
    tapply(ad_long$age, list(ad_long$modal_age, ad_long[[by]]), cv) %>%
      unclass %>%
      as.data.frame %>%
      mutate(all = tapply(ad_long$age, list(ad_long$modal_age), cv) %>% unclass %>% unname)
  cv_tab[modal_age_range == 0,] <- NA

  # Add weighted mean
  num_read <- numbers_read(ad_long, by)
  cv_tab <- rbind(cv_tab,
                  colSums(cv_tab * num_read, na.rm = TRUE) / colSums(num_read * !is.na(cv_tab), na.rm = TRUE))

  # produce formatted version
  format_table(cv_tab, fmt = "%.0f %%", extra_rows = "Weighted Mean")
}


ape_table <- function(ad_long, by = "reader") {

  ape_tab <-
    tapply(ad_long$age, list(ad_long$modal_age, ad_long[[by]]), ape) %>%
      unclass %>%
      as.data.frame %>%
      mutate(all = tapply(ad_long$age, list(ad_long$modal_age), ape) %>% unclass %>% unname)
  ape_tab[modal_age_range == 0,] <- NA

  # Add weighted mean
  num_read <- numbers_read(ad_long, by)
  ape_tab <- rbind(ape_tab,
                  colSums(ape_tab * num_read, na.rm = TRUE) / colSums(num_read * !is.na(ape_tab), na.rm = TRUE))

  # produce formatted version
  format_table(ape_tab, fmt = "%.0f %%", extra_rows = "Weighted Mean")
}

pa_table <- function(ad_long, by = "reader") {

  pa_tab <-
    ad_long %>%
    filter(age == modal_age) %>%
    with(., table(modal_age, .[[by]])) %>%
    unclass %>%
    as.data.frame %>%
    mutate(
      all = rowSums(.)
    )

  # total numbers read
  num_read <- numbers_read(ad_long, by)

  # overall agreement per modal age
  pa_tab <- pa_tab / num_read * 100

  # Add weighted mean
  pa_tab <- rbind(pa_tab,
                  colSums(pa_tab * num_read, na.rm = TRUE) / colSums(num_read * !is.na(pa_tab), na.rm = TRUE))

  # produce formatted version
  format_table(pa_tab, fmt = "%.0f %%", extra_rows = "Weighted Mean")
}


rel_bias_table <- function(ad_long, by = "reader") {

  rel_bias_tab <-
    tapply(ad_long$age, list(ad_long$modal_age, ad_long[[by]]), mean, na.rm = TRUE) %>%
      unclass %>%
      as.data.frame %>%
      mutate(
        all = rowMeans(.)
      )

  # calculate difference per reader: mean age - modal age
  rel_bias_tab <- rel_bias_tab - modal_age_range

  # Add weighted mean
  num_read <- numbers_read(ad_long, by)
  rel_bias_tab <-
    rbind(rel_bias_tab,
          colSums(rel_bias_tab * num_read, na.rm = TRUE) / colSums(num_read * !is.na(rel_bias_tab), na.rm = TRUE))

  # produce formatted version
  format_table(rel_bias_tab, fmt = "%.2f", extra_rows = "Weighted Mean")
}


age_composition <- function(ad_long, by = "reader") {
# Number of otoliths read per reader and age
  ad_long %>%
    with(., table(age, .[[by]])) %>%
    unclass %>%
    as.data.frame
}


age_composition_table <- function(ad_long, by = "reader") {
  if (nrow(ad_long) == 0) return(data.frame("Modal age" = numeric(0)))
  # Number of otoliths read per reader and age
  ad_long %>%
    age_composition(by = by)  %>%
    rbind(colSums(., na.rm = TRUE)) %>%
    format_table(age_range = sort(unique(ad_long$age)))
}



mean_length_table <- function(ad_long, by = "reader") {
  if (nrow(ad_long) == 0) return(data.frame("Modal age" = numeric(0)))
  # Fish mean length per age and reader
  mean_length_tab <-
    ad_long %>%
    with(., tapply(length, list(age, .[[by]]), mean, na.rm = TRUE)) %>%
    unclass %>%
    as.data.frame

  # Calculate weighted mean length per reader
  age_comp <-  age_composition(ad_long, by)
  mean_length_tab <-
    rbind(mean_length_tab,
          colSums(mean_length_tab * age_comp, na.rm = TRUE) / colSums(age_comp, na.rm = TRUE))


  # Re-naming and combine mean lengths per reader and age with weighted means
  format_table(mean_length_tab, fmt = "%.0f mm",
               extra_rows = "Weighted Mean",
               age_range = sort(unique(ad_long$age))) %>%
    rename(Age = `Modal age`)

}


data_overview_table <- function(ad_long, report_token, by = "stock") {

  # Select only columns of age readings
  ad_wide <-
    ad_long %>%
    select(FishID, length, sex, catch_date, ices_area, reader, age) %>%
    spread(key = reader, value = age)

  # Calculate, modeal age, percentage agreement, cv, and ape
  readings <-
    ad_wide %>%
    select(matches("R[0-9][0-9]*"))

  ad_wide$`Modal age` <- apply(readings, 1, Mode)
  ad_wide$`PA %` <- round(rowMeans(readings == ad_wide$`Modal age`, na.rm = TRUE)*100)
  ad_wide$`CV %` <- round(apply(readings, 1, cv))
  ad_wide$`APE %` <- round(apply(readings, 1, ape))
  ad_wide <- rename(ad_wide, `ICES area` = ices_area, `Catch date` = catch_date)

  ad_wide$`CV %`[is.nan(ad_wide$`CV %`)] <- NA
  ad_wide$`APE %`[is.nan(ad_wide$`APE %`)] <- NA

  # add hyper link for tables
  ad_long %>%
    group_by(FishID, EventID) %>%
    summarise(
      `Image ID` = sprintf("[%s](http://smartdots.ices.dk/viewImage?tblEventID=%i&SmartImageID=%s&token=%s)",
                         sample, EventID, sample, report_token) %>%
                unique %>%
                paste(collapse = "-")
    ) %>%
    right_join(ad_wide, by = "FishID") %>%
    rename(
      `Fish ID` = FishID,
      `Event ID` = EventID
    ) %>%
    as.data.frame
}




# Inter Reader Bias Test ######################################################

# This test compares the bias pairwise between readers and between readers
# and modal age.
# The bias test is made according to the analysis in the GE sheet.
# We have not been able to find the theory behind this method
# and all equaitons and calculations are therfore taken directly from GE..

bias_test <- function(ad_long) {

  # age readings
  ages <-
    ad_long %>%
    select(FishID, modal_age, reader, age) %>%
    spread(key = reader, value = age) %>%
    select(modal_age, matches("R[0-9][0-9] *"))

  dat_un <- ages

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

age_er_matrix <- function(ad_long, by = NULL) {
  # Relative contribution of each age per modal age (long format)
  ad_long %>%
    group_by_at(c("modal_age", "age", by)) %>%
    summarise(
      age_per_modal = sum(!is.na(age))
    ) %>%
    ungroup %>%
    group_by_at(c("modal_age", by)) %>%
    mutate(
      total_per_modal = sum(age_per_modal)
    ) %>%
    ungroup %>%
    mutate(
      rel_age = age_per_modal / total_per_modal
    ) %>%
    select_at(c(by, "age", "modal_age", "rel_age")) %>%
    spread(modal_age, rel_age) %>%
    mutate(age = paste("Age", age)) %>%
    rename(`Modal age` = age) %>%
    as.data.frame %>%
    # split into several data.frames?
    by(apply(.[by], 1, paste, collapse = ", "), function(x) {rownames(x) <- NULL; x}) %>%
      unclass %>%
      (function(x) {
        attr(x, "call") <- NULL
        x
      })
}
