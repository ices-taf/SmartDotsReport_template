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



get_ages <- function(dat_in){

   cols <- which(names(dat_in) == "modal_age"):ncol(dat_in)
   dat_out <- dplyr::select(dat_in, cols)
  return(dat_out)
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
