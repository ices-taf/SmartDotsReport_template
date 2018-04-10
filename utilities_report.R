
library(ggplot2)
library(reshape)
library(dplyr)
library(tidyr)
library(pander)
library(scales)
library(pipeR)


# plot_rb_ma      : Plot of relative bias per modal age
# plot_growth     : Function to plot growth box plots per reader and modal age
# plot_bias       : Bias plot for each reader. Mean ages are plotted against
#                   the modal age
# plot_bias_all   : Bias plot for all readers combines.


# Style output tables #########################################################

# These four functions are used to change the style of the output tables.
# Depending on the form of the table different styels are chiosen.

# Style 0
style_table0 <- function(tab) {

  # Capitalize first letter of column and make header boldface
  names(tab) <- capFirst(names(tab))
  names(tab) <- pandoc.strong.return(names(tab))

  return(tab)
}

# Style 1
style_table1 <- function(tab) {

  # Capitalize first letter of column, make header, last column and second
  # last row in boldface and make last row italic
  names(tab) <- capFirst(names(tab))
  names(tab) <- pandoc.strong.return(names(tab))
  emphasize.strong.cols(ncol(tab))
  emphasize.strong.rows(nrow(tab))

  return(tab)
}

# Style 2
style_table2 <- function(tab) {

  # Capitalize first letter of column, make header, last column and
  # last row in boldface
  names(tab) <- capFirst(names(tab))
  names(tab) <- pandoc.strong.return(names(tab))
  emphasize.strong.cols(ncol(tab))
  emphasize.strong.rows(nrow(tab))

  return(tab)
}

# Style 3
style_table3 <- function(tab) {

  # Capitalize first letter of column, make header and first column boldface
  names(tab) <- capFirst(names(tab))
  names(tab) <- pandoc.strong.return(names(tab))
  emphasize.strong.cols(1)

  return(tab)
}



# Age bias plots  #############################################################


# The age bias plots are made per reader.
# For each reader is the mean age and 2 times standard deviation
# (as error bars) plotted against the modal age.

plot_bias <- function(dat_in,max_age,max_modal){

  for (i in sort(unique(dat_in$reader)) ) {

    # Set plot frame
    p <- ggplot(dat_in[dat_in$reader==i,],
                aes(modal_age, age, group = reader), color="#80B1D3")

    # Plot data and make settings/design
    p2 <- p + stat_summary(fun.data = mean_sdl, geom = "pointrange",
                           na.rm = TRUE, color = "#80B1D3") +
            geom_abline(colour = "grey70") +
            facet_wrap(~reader, ncol = 2) +
            xlab("Modal age") + ylab("Mean age +/- 2 stdev") + theme_bw()+
            theme(axis.title = element_text(size = rel(0.9)),
                  axis.text = element_text(size = rel(0.8)),
                  panel.grid.minor = element_blank(),
                  strip.background = element_rect(colour = "black",
                                                  fill = "white"),
                  strip.text = element_text(face = "bold", size = 10)) +
            scale_x_continuous(breaks = seq(0, max_modal, 1),
                               limits = c(0, max_modal), oob = rescale_none) +
            scale_y_continuous(breaks = seq(0, max_age + 1, 1),
                               limits = c(-1, max_age + 1),
                               oob = rescale_none) +
            theme(plot.margin = unit(c(0.8, 0.2, 0, 0), "cm"))

     plot(p2)


  }


}


# Age bias plot for all readers combined ######################################


# Here the age bias are plotted for all readers combined.

plot_bias_all <- function(dat_in,max_age,max_modal){

  # Make variable for facet
  dat_in$group_in <- "All readers"

  # Set plot frame
  p <- ggplot(dat_in,
              aes(modal_age, age, group = group_in), color="#80B1D3")

  # Plot data and make settings/design
  p2 <- p + stat_summary(fun.data = mean_sdl, geom = "pointrange",
                         na.rm = TRUE, color = "#80B1D3") +
    geom_abline(colour = "grey70") +
    facet_wrap(~group_in, ncol = 1) +
    xlab("Modal age") + ylab("Mean age +/- 2 stdev") + theme_bw()+
    theme(axis.title = element_text(size = rel(0.9)),
          axis.text = element_text(size = rel(0.8)),
          panel.grid.minor = element_blank(),
          strip.background = element_rect(colour = "black",
                                          fill = "white"),
          strip.text = element_text(face = "bold", size = 10)) +
    scale_x_continuous(breaks = seq(0, max_modal, 1),
                       limits = c(0, max_modal), oob = rescale_none)+
    scale_y_continuous(breaks = seq(0, max_age + 1, 1),
                       limits = c(-1, max_age + 1), oob = rescale_none)+
    theme(plot.margin = unit(c(0.8, 0.2, 0, 0), "cm"))

  plot(p2)


}


# Plot std, ca and pa #########################################################

# Plot overall std, CA and PA per modal age in same plot
plot_stat <- function(std, pa, cv){

  # Combine the three data sets
  dat_in <- cbind(std[, c("modal_age", "sd")], "pa" = pa[,-1], "cv" = cv[,-1])
  dat_in[dat_in$modal_age == 0,"cv"] <- NA # Not consider CV for age 0

  # Limit to use for axis
  std_lim <- ceiling(max(dat_in$sd, na.rm = T))

  gp1 <- dat_in[!is.na(dat_in$sd),] %>>% ggplot() + theme_bw() +
         # Standard deviation
         geom_line(aes(x = modal_age, y = sd, colour = "sd")) +
         geom_point(aes(x = modal_age, y = sd, colour = "sd",
                        shape = "sd"), size = 3) +
         # CV
         geom_line(aes(x = modal_age, y = cv*std_lim/100, colour = "cv")) +
         geom_point(aes(x = modal_age, y = cv*std_lim/100, colour = "cv",
                        shape = "cv"), size = 3) +
         # PA
         geom_line(aes(x = modal_age, y = pa*std_lim/100, colour = "pa")) +
         geom_point(aes(x = modal_age, y = pa*std_lim/100, colour = "pa",
                        shape = "pa"), size = 3) +
         # Make left side y-axis
         scale_y_continuous(name = expression("Standard deviation (years)"),
                            limits = c(0, std_lim))  +
         # Make right side y-axis
         scale_y_continuous(name = expression("Standard deviation (years)"),
                                  sec.axis = sec_axis(~ . * 100/std_lim,
                                  name = "CV & PA (%)"),
                                  limits = c(0, std_lim))

  # Colors and labels
  gp2 <- gp1 + theme(axis.text.y = element_text(color = "#80B1D3"),
                  axis.text.y.right = element_text(color = "#FB8072")) +
         scale_colour_manual(name = "Measure",
                              values = c("#FB8072", "#FB8072", "#80B1D3"),
                              labels = c("CV", "PA", "STDEV")) +
         scale_shape_manual(name = "Measure", values = c(16, 8, 17),
                              labels = c("CV", "PA", "STDEV")) +
         labs(x = "Modal age", colour = "")

  return(gp2)

}



# Plot mean length at age by reader ###########################################

# For each reader and modal age plot mean length
plot_mla <- function(dat_in){

  # x-axis text size if large number of readers
  text_x <- ifelse(length(unique(dat_in$reader)) > 20, 5, 8)

  # Data is grouoed per age and each age has its own color in plotting
  p <- ggplot(dat_in[!is.na(dat_in$age),],
              aes(x = factor(reader), y = mean_len, group = factor(age))) +
       geom_line(aes(color = factor(age))) +
       geom_point(aes(color = factor(age)), size = 2)+
       scale_colour_brewer("Age", palette = "Set3") +
       theme_bw() + labs(x = "Reader", y = "Mean length (mm)")+
       theme(axis.text.x = element_text(size = text_x, angle = 30,
                                        vjust = 0.9, hjust = 1.01))

  return(p)

}

# Plot relative distribution of ages ##########################################

# For each modal age plot the relation contribution of each age

plot_rdist <- function(dat_in,ma){

  difs <- melt(dat_in, id.vars = c("modal_age"))

  p <- ggplot(difs[difs$modal_age %in% ma,],
              aes(x = variable, y = value/100, colour = factor(modal_age),
              group = factor(modal_age)))+
       geom_point(size = 2) + geom_line()+
       scale_colour_brewer("Modal age", palette = "Set3") +
       theme_bw() + labs(x = "Age error", y = "Frequency") +
       scale_y_continuous(labels = percent)


  return(p)

}


# Plot relative bias of ages ##################################################

# Plot of the overall bias per modal age

plot_rb_ma <- function(dat_in){

  p <- ggplot(dat_in[!is.na(dat_in$all),],
              aes(x = modal_age, y = all, group = 1))+
       geom_point(size = 2, colour = "#80B1D3") +
       geom_line(colour = "#80B1D3") +
       geom_abline(slope = 0, intercept = 0, colour = "#FB8072",
                   lty = "dashed") +
       theme_bw() + labs( x = "Modal age", y = "Relative bias")

  return(p)

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
