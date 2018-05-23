
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

get_ages <- function(dat_in){

   cols <- which(names(dat_in) == "modal_age"):ncol(dat_in)
   dat_out <- dplyr::select(dat_in, cols)
  return(dat_out)
}


# Table 1 #####################################################################

# Creates Table 1 in GE with sample info, age readings, modal age, CV and PA
# Additionally APE is also added to the output (not part of GE table 1)
data_ov <- function(dat_in, dat_in2, event_id, report_token){


  # Select only columns of age readings
  readings <- get_ages(dat_in)

  # Calculate percentage agreement
  dat_in$perc_agree <- round2(rowSums(readings[, -1] == readings$modal_age,
                                      na.rm = TRUE)/
                                      rowSums(!is.na(readings[, -1]))*100)

  # Combine data in correct order

  sample_vars <- select(dat_in, c(sample, length, sex, catch_date,
                                  ices_area, stock))

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
  sum_stat <- data.frame("Mean PA %" = round2(meanNA(dat_out2$perc_agree)),
                         "Mean CV %" = round2(meanNA(dat_out2$cv)),
                         "Mean APE %" = round2(meanNA(dat_out2$ape)))
  colnames(sum_stat) <- c("Mean PA %", "Mean CV %", "Mean APE %")

  # Renaming selected columns
  date_vals <- "catch_date"
  date_name <- "Catch date"

  names(dat_out2)[names(dat_out2) %in%
                   c("sample", "length", "sex", date_vals, "ices_area", "stock",
                     "modal_age", "perc_agree",'cv', 'ape')] <-
                   c("Sample", "Length (mm)", "Sex", date_name,"ICES area",
                     "ICES stock", "Modal age", "PA %", "CV %", "APE %")

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



# Growth analysis #############################################################

# Plot
# plot_growth(dist, compl_sample, part_tab2, "Expert")
# compl <- compl_sample; part <- part_tab2; exp <- "Expert"
plot_growth <- function(dist, compl, part,exp) {

  gro_dat  <- dist[dist$sample %in% compl,] %>%
              mutate(meas_dist = distance,  # to keep original measurements
                     winterring = mark - 1,
                     readerXimage = as.factor(paste(
                       reader1, sample, sep = "-"))) %>% # true annulus, remove mark = 0
              filter(mark != 0 & distance != 0)

  gro_dat1 <- gro_dat[ order(gro_dat$readerXimage, gro_dat$winterring), ]
  # Add subsequent growth increments to growth curves by reader
  gro_dat1$distance <- ave(gro_dat1$distance, gro_dat1$readerXimage, FUN = cumsum)

  if (exp == "Expert") {
    # Merge with readers2 and remove readers if an expertise level is used
    gro_dat2 <- merge(gro_dat1, part[, c("reader", "expertise")], all.x = TRUE)
    gro_dat1 <- gro_dat2[gro_dat2$expertise == "Advanced",]
  }

  gro_dat1$Reader <- factor(gro_dat1$reader)
  gro_dat1 <- select(gro_dat1, winterring, distance, Reader)

  # add fake data
  fake <- expand.grid(winterring = unique(gro_dat1$winterring), Reader = levels(gro_dat1$Reader))
  fake <- anti_join(fake, unique(select(gro_dat1, -distance)))
  if (nrow(fake)) {
    fake$distance <- max(gro_dat1$distance) * 2
    gro_dat3 <- rbind(gro_dat1, fake)
  } else {
    gro_dat3 <- gro_dat1
  }

  p_all <-
    ggplot(gro_dat3,
           aes(x = factor(winterring + 1), y = distance, col = Reader))
  p <-
    p_all +
    geom_boxplot(lwd = 0.2, cex = 0.7) +
    xlab("Annulus") +
    ylab("Distance from center (mm)")+
    theme_bw() +
    theme(axis.text = element_text(size = 12),
          axis.title = element_text(size = 14),
          legend.title = element_text(size = 12)) +
    coord_cartesian(ylim = range(gro_dat1$distance) + c(-.25, .25))

  p
}

