

format_table <- function(tab, fmt = "%i", extra_rows = "Total", age_range = modal_age_range) {
  tab[] <- lapply(tab, function(x) ifelse(is.na(x), "-", sprintf(fmt, x)))
  cbind(`Modal age` = c(age_range, extra_rows), tab)
}


sample_data_overview_table <- function(ad_long, strata) {
  sample_data_overview <-
    ad_long %>%
    select_at(unique(c("FishID", "SampleID", "length", "modal_age", "year", "ices_area", "qtr", strata, "reader", "age"))) %>%
    spread(key = reader, value = age) %>%
    group_by_at(unique(c("year", "ices_area", "qtr", strata))) %>%
    summarise(
      min_len = 5*round(min(length, na.rm = TRUE)/5),
      max_len = 5*round(max(length, na.rm = TRUE)/5),
      num = sum(!is.na(SampleID)),
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


# Participants table
reader_data_table <- function(ad_long, strata){
  ad_long %>%
  select_at(unique(c("reader", "expertise", "reader_number", strata))) %>%
  unique %>%
  arrange(reader) %>%
  rename(`Reader code` = reader,
         Expertise = expertise,
         Expertise_rank = reader_number)
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



# Prepare the table with the number of cases with multiple modes depending if the methodology used to define the mode is the traditional mode (no weighting of the readers), using a linear weighting for the readers, a negative exponential weighting or a multistage approach, where a combination of the different approaches is used.
multimode_cases_table_traditional<- function(ad_long) {

  MM_tab=tapply(ad_long$NModes_trad,ad_long$SampleID,max) %>% unclass %>% as.data.frame
  colnames(MM_tab)=c("NModes_trad")
  MM_tab$SampleID=rownames(MM_tab)
  
  MM_tab=MM_tab[MM_tab$NModes_trad>1,]
  if(dim(MM_tab)[1]==0) {MM_tab=data.frame(SampleID="none", NModes_trad="zero")}
  return(MM_tab)
}

multimode_cases_table_linear <- function(ad_long, by = "reader") {
  
  MM_tab=tapply(ad_long$NModes_linear,ad_long$SampleID,max) %>% unclass %>% as.data.frame
  colnames(MM_tab)=c("NModes_linear")
  MM_tab$SampleID=rownames(MM_tab)
  
  MM_tab=MM_tab[MM_tab$NModes_linear>1,]
  if(dim(MM_tab)[1]==0) {MM_tab=data.frame(SampleID="none", NModes_linear="zero")}
  return(MM_tab)
}

multimode_cases_table_negexp <- function(ad_long, by = "reader") {
  
  MM_tab=tapply(ad_long$NModes_negexp,ad_long$SampleID,max) %>% unclass %>% as.data.frame
  colnames(MM_tab)=c("NModes_negexp")
  MM_tab$SampleID=rownames(MM_tab)
  
  MM_tab=MM_tab[MM_tab$NModes_negexp>1,]
  if(dim(MM_tab)[1]==0) {MM_tab=data.frame(SampleID="none", NModes_negexp="zero")}
  return(MM_tab)
}

# in the case of the multistage approach, the number of multiple mode cases will be the same than the negative exponential weighting approach.
multimode_cases_table_multistage <- function(ad_long, by = "reader") {
  
  MM_tab=tapply(ad_long$NModes_negexp,ad_long$SampleID,max) %>% unclass %>% as.data.frame
  colnames(MM_tab)=c("NModes_multistage")
  MM_tab$SampleID=rownames(MM_tab)
  
  MM_tab=MM_tab[MM_tab$NModes_multistage>1,]
  if(dim(MM_tab)[1]==0) {MM_tab=data.frame(SampleID="none", NModes_multistage="zero")}
  return(MM_tab)
}


cv_table <- function(ad_long, by = "reader") {

  cv_tab <-
    tapply(ad_long$age, list(ad_long$modal_age, ad_long[[by]]), cv_II) %>%
      unclass %>%
      as.data.frame %>%
      mutate(all = tapply(ad_long$age, list(ad_long$modal_age), cv_II) %>% unclass %>% unname)
  # ape_tab[modal_age_range == 0,] <- NA

  # Add weighted mean
  num_read <- numbers_read(ad_long, by)
  cv_tab <- rbind(cv_tab,
                  colSums(cv_tab * num_read, na.rm = TRUE) / colSums(num_read * !is.na(cv_tab), na.rm = TRUE))
  
  # We don't want to present any CV estimate for modal age 0
  cv_tab[rownames(cv_tab)=="0",]=NA

  # produce formatted version
  format_table(cv_tab, fmt = "%.0f %%", extra_rows = "Weighted Mean")
}



ape_table <- function(ad_long, by = "reader") {

  ape_tab <-
    tapply(ad_long$age, list(ad_long$modal_age, ad_long[[by]]), ape) %>%
      unclass %>%
      as.data.frame %>%
      mutate(all = tapply(ad_long$age, list(ad_long$modal_age), ape) %>% unclass %>% unname)
  # ape_tab[modal_age_range == 0,] <- NA

  # Add weighted mean
  num_read <- numbers_read(ad_long, by)
  ape_tab <- rbind(ape_tab,
                  colSums(ape_tab * num_read, na.rm = TRUE) / colSums(num_read * !is.na(ape_tab), na.rm = TRUE))

  # We don't want to present any CV estimate for modal age 0
  ape_tab[rownames(ape_tab)=="0",]=NA
  
  # produce formatted version
  format_table(ape_tab, fmt = "%.0f %%", extra_rows = "Weighted Mean", age_range = modal_age_range)
}


pa_table <- function(ad_long, by = "reader") {

  pa_tab <-
    ad_long %>%
    filter(age == modal_age) %>%
    with(., table(modal_age, .[[by]])) %>%
    unclass %>%
    as.data.frame %>%
    mutate(
      total = rowSums(.)
    )

  # total numbers read
  num_read <- numbers_read(ad_long, by)

  # #It may be that some readers never agreed with the modal age/maturity stage. This would create differences with the dimensions of pa_tab and num_read. Next, this problem is solved by forcing pa_tab to have the same columns than num_read.
  A=colnames(pa_tab)
  B=colnames(num_read)
  diff=c(setdiff(A,B), setdiff(B,A))
  temp=as.data.frame(matrix(data=0,nrow=dim(pa_tab)[1],ncol=length(diff), dimnames = list(c(1:dim(pa_tab)[1]), diff)))
  pa_tab=cbind(pa_tab,temp)
  
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
  rel_bias_tab <- rel_bias_tab - as.numeric(levels(ad_long$modal_age))

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
  tabla <- ad_long %>%
    age_composition(by = by) %>%
    mutate(total = unname(rowSums(.)))  %>%
    rbind(colSums(., na.rm = TRUE)) %>%
    format_table(age_range = sort(unique(ad_long$age)))
  colnames(tabla)[1]="Age"
  return(tabla)
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
    select(FishID, SampleID, length, sex, catch_date, ices_area, reader, age) %>%
    spread(key = reader, value = age)

  temp <-
    ad_long %>%
    select(FishID, modal_age) %>%
    distinct()
     
  
  dim(temp)
  dim(ad_wide)
  ad_wide=merge(ad_wide, temp, by.x="FishID", by.y="FishID")
  ad_wide$modal_age=as.numeric(as.character(ad_wide$modal_age))
  dim(ad_wide)
  
  
  # Calculate, modeal age, percentage agreement, cv, and ape
  readings <-
    ad_wide %>%
    select(matches("R[0-9][0-9]*"))

  # ad_wide$`Modal age` <- apply(readings, 1, Mode_II)
  ad_wide$`PA %` <- round(rowMeans(readings == ad_wide$`modal_age`, na.rm = TRUE)*100)
  ad_wide$`CV %` <- round(apply(readings, 1, cv_II))
  ad_wide$`APE %` <- round(apply(readings, 1, ape))
  ad_wide <- rename(ad_wide, `ICES area` = ices_area, `Catch date` = catch_date, `Modal age` = modal_age)

  ad_wide$`CV %`[is.nan(ad_wide$`CV %`)] <- NA
  ad_wide$`APE %`[is.nan(ad_wide$`APE %`)] <- NA

  # add hyper link for tables
  ad_long %>%
    group_by(FishID, SampleID, EventID) %>%
    summarise(
      `Image ID` = sprintf("[%s](http://smartdots.ices.dk/viewImage?tblEventID=%i&SmartImageID=%s&token=%s)",
                         sample, EventID, sample, report_token) %>%
                unique %>%
                paste(collapse = "-")
    ) %>%
    right_join(ad_wide, by = c("FishID", "SampleID")) %>%
    rename(
      `Event ID` = EventID,
      `Fish ID` = FishID,
      `Sample ID` = SampleID
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
    select(FishID, SampleID, modal_age, reader, age) %>%
    spread(key = reader, value = age) %>%
    select(modal_age, matches("R[0-9][0-9] *"))

  dat_un <- ages
  dat_un$modal_age <- as.numeric(as.character(dat_un$modal_age))

  # For each pair of readers (and each reader compared to th modal age of
  # the sample) and each image get reading difference
  diffs <- outer(1:ncol(dat_un), 1:ncol(dat_un),
                 function(x, y) dat_un[, y] - dat_un[, x])
  colnames(diffs) <- outer(colnames(dat_un),
                           colnames(dat_un), paste, sep = ":")

  #Prepare inter reader bias table
  int_bias <- data.frame(matrix(NA, nrow = length(dat_un),
                                 ncol = length(dat_un) - 1))
  colnames(int_bias) <- colnames(dat_un)[-1]
  rownames(int_bias) <- c(colnames(dat_un)[-1], "modal_age")

  #Loop through each pair
  for (i in 1:ncol(diffs)) {

    # Frequency table of the differnces between each reader combination
    data <- data.frame(table(diffs[[i]]))
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

  int_bias2[is.na(int_bias2)]="-"
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
