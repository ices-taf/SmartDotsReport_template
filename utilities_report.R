# Style output tables #########################################################

# These four functions are used to change the style of the output tables.
# Depending on the form of the table different styels are chiosen.

# Style 0
style_table0 <- function(tab) {

  # Capitalize first letter of column and make header boldface
  names(tab) <- pandoc.strong.return(names(tab))

  return(tab)
}

# Style 1
style_table1 <- function(tab) {

  # Capitalize first letter of column, make header, last column and second
  # last row in boldface and make last row italic
  names(tab) <- pandoc.strong.return(names(tab))
  emphasize.strong.cols(ncol(tab))
  emphasize.strong.rows(nrow(tab))

  return(tab)
}

# Style 2
style_table2 <- function(tab) {

  # Capitalize first letter of column, make header, last column and
  # last row in boldface
  names(tab) <- pandoc.strong.return(names(tab))
  emphasize.strong.cols(ncol(tab))
  emphasize.strong.rows(nrow(tab))

  return(tab)
}

# Style 3
style_table3 <- function(tab) {

  # Capitalize first letter of column, make header and first column boldface
  names(tab) <- pandoc.strong.return(names(tab))
  emphasize.strong.cols(1)

  return(tab)
}




# Here the age bias are plotted for all readers combined.

plot_bias_all <- function(ad_long, max_age, max_modal, sel_readers){

  # limits
  max_modal <- max(as.numeric(as.character(ad_long$modal_age, na.rm = TRUE)))
  max_age <- max(ad_long$age, na.rm = TRUE)

  p <-
    ad_long %>%
    # Make variable for facet
    mutate(group_in = sel_readers) %>%
    ggplot(aes(modal_age, age, group = group_in), color="#80B1D3")

  # Plot data and make settings/design
  p +
    stat_summary(fun.data = mean_sdl, geom = "pointrange", na.rm = TRUE, color = "#80B1D3") +
    geom_abline(colour = "grey70") +
    facet_wrap(~group_in, ncol = 1) +
    xlab("Modal age") + ylab("Mean age +/- 2 stdev") +
    theme_bw() +
    theme(axis.title = element_text(size = rel(0.9)),
          axis.text = element_text(size = rel(0.8)),
          panel.grid.minor = element_blank(),
          strip.background = element_rect(colour = "black",
                                          fill = "white"),
          strip.text = element_text(face = "bold", size = 10)
          #text = element_text(family="Calibri")
          ) +
    scale_x_continuous(breaks = seq(0, max_modal, 1),
                       limits = c(0, max_modal), oob = rescale_none)+
    scale_y_continuous(breaks = seq(0, max_age + 1, 1),
                       limits = c(-1, max_age + 1), oob = rescale_none)+
    theme(plot.margin = unit(c(0.8, 0.2, 0, 0), "cm"))

}

# Growth analysis #############################################################

plot_growth <- function(dist, ad_long, stratif = NULL) {

  if(stratif=="no_stratification")
  {
    p <-
      dist %>%
      arrange(reader, AnnotationID, mark) %>%
      filter(mark > 0, distance > 0, reader %in% unique(ad_long$reader)) %>%
      group_by(reader, AnnotationID) %>%
      ungroup %>%
      #    select(AnnotationID, mark, distance, cum_distance, reader) %>%
      mutate(
        Annulus = factor(mark),
        Reader = factor(reader)
      ) %>%
      left_join(ad_long) %>%  # add by here
      select_at(c("Annulus", "distance", "prep_method", "Reader")) %>%
      ggplot(aes(x = Annulus, y = distance, col = Reader)) +
      geom_boxplot() +
      xlab("Annulus") +
      ylab("Distance from center (mm)")+
      theme_bw() +
      theme(axis.text = element_text(size = 12),
            axis.title = element_text(size = 14),
            legend.title = element_text(size = 12))
    p
  } else { if(stratif=="strata") {
    p <-
      dist %>%
      arrange(reader, AnnotationID, mark) %>%
      filter(mark > 0, distance > 0, reader %in% unique(ad_long$reader)) %>%
      group_by(reader, AnnotationID) %>%
      ungroup %>%
      mutate(
        Annulus = factor(mark),
        Reader = factor(reader)
      ) %>%
      left_join(ad_long) %>%  # add by here
      select_at(c("Annulus", "distance", "prep_method", "Reader", "strata")) %>%
      ggplot(aes(x = Annulus, y = distance, col = Reader)) +
      geom_boxplot() + 
      facet_wrap(~strata, dir="v", scales="free_y") +
      xlab("Annulus") +
      ylab("Distance from center (mm)")+
      theme_bw() +
      theme(axis.text = element_text(size = 12),
            axis.title = element_text(size = 14),
            legend.title = element_text(size = 12))
    p
  } else {
    p <-
      dist %>%
      arrange(reader, AnnotationID, mark) %>%
      filter(mark > 0, distance > 0, reader %in% unique(ad_long$reader)) %>%
      group_by(reader, AnnotationID) %>%
      ungroup %>%
      #    select(AnnotationID, mark, distance, cum_distance, reader) %>%
      mutate(
        Annulus = factor(mark),
        Reader = factor(reader)
      ) %>%
      left_join(ad_long) %>%  # add by here
      select_at(c("Annulus", "distance", "prep_method", "Reader", "strata")) %>%
      subset(strata==stratif) %>%
      #ggplot(aes(x = Annulus, y = cum_distance, col = Reader)) +
      ggplot(aes(x = Annulus, y = distance, col = Reader)) +
      geom_boxplot()
    
    # formatting
    p +
      xlab("Annulus") +
      ylab("Distance from center (mm)")+
      theme_bw() +
      theme(axis.text = element_text(size = 12),
            axis.title = element_text(size = 14),
            legend.title = element_text(size = 12))
    # + theme(aspect.ratio = 0.8)
    # don;t show fake data
    #coord_cartesian(ylim = range(growth_dat$distance) + c(-.25, .25))
  }
 }
}



# Age bias plots  #############################################################


# The age bias plots are made per reader.
# For each reader is the mean age and 2 times standard deviation
# (as error bars) plotted against the modal age.

plot_bias <- function(ad_long) {
  # limits
  max_modal <- max(ad_long$modal_age, na.rm = TRUE)
  max_age <- max(ad_long$age, na.rm = TRUE)

  # return list of plots
  lapply(sort(unique(ad_long$reader)), function(ireader) {
    # Plot data and make settings/design
    ad_long %>%
      filter(reader == ireader) %>%
      ggplot(aes(modal_age, age, group = reader), color="#80B1D3") +
        stat_summary(fun.data = mean_sdl, geom = "pointrange", na.rm = TRUE, color = "#80B1D3") +
        geom_abline(colour = "grey70") +
        facet_wrap(~ reader, ncol = 2) +
        xlab("Modal age") + ylab("Mean age +/- 2 stdev") + theme_bw()+
        theme(axis.title = element_text(size = rel(0.9)),
              axis.text = element_text(size = rel(0.8)),
              panel.grid.minor = element_blank(),
              strip.background = element_rect(colour = "black", fill = "white"),
              strip.text = element_text(face = "bold", size = 10)
              #text = element_text(family="Calibri")
              ) +
              scale_x_continuous(breaks = seq(0, max_modal, 1),
                                 limits = c(0, max_modal), oob = rescale_none) +
              scale_y_continuous(breaks = seq(0, max_age + 1, 1),
                                 limits = c(-1, max_age + 1),
                                 oob = rescale_none) +
        theme(plot.margin = unit(c(0.8, 0.2, 0, 0), "cm"))
  })
}



# Plot std, ca and pa #########################################################

# Plot overall std, CA and PA per modal age in same plot
plot_stat <- function(ad_long) {

  max_modal <- max(ad_long$modal_age, na.rm = TRUE)
  max_age <- max(ad_long$age, na.rm = TRUE)

  # Combine the three data sets
  dat_in <-
    ad_long %>%
      group_by(modal_age) %>%
      summarise(
        cv = cv_II(age),
        pa = mean(age == modal_age, na.rm = TRUE) * 100,
        sd = sd(age, na.rm = TRUE)
      )

  # Limit to use for axis
  std_lim <- ceiling(max(dat_in$sd, na.rm = T))

  p <-
    dat_in %>%
    filter(!is.na(sd)) %>%
    ggplot() +
    theme_bw() +
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
    scale_x_continuous(breaks = seq(0, max_modal, 1),
                       limits = c(0, max_modal), oob = rescale_none) +
    # Make left side y-axis
    scale_y_continuous(name = expression("Standard deviation (years)"),
                      limits = c(0, std_lim))  +
    # Make right side y-axis
    scale_y_continuous(name = expression("Standard deviation (years)"),
                            sec.axis = sec_axis(~ . * 100/std_lim,
                            name = "CV & PA (%)"),
                            limits = c(0, std_lim))

  # Colors and labels
  p +
    theme(axis.text.y = element_text(color = "#80B1D3"),
          axis.text.y.right = element_text(color = "#FB8072")
          #text = element_text(family="Calibri")
          ) +
    scale_colour_manual(name = "Measure",
                        values = c("#FB8072", "#FB8072", "#80B1D3"),
                        labels = c("CV", "PA", "STDEV")) +
    scale_shape_manual(name = "Measure", values = c(16, 8, 17),
                       labels = c("CV", "PA", "STDEV")) +
    labs(x = "Modal age", colour = "")
}





plot_rdist <- function(ad_long) {
  ad_long %>%
    select(age, modal_age) %>%
    mutate(diff_age = age - modal_age) %>%
    with(., table(diff_age, modal_age)) %>%
    as.data.frame %>%
    group_by(modal_age) %>%
    mutate(prop = Freq / sum(Freq)) %>%
    ggplot(aes(x = diff_age, y = prop, colour = factor(modal_age), group = factor(modal_age))) +
      geom_point(size = 2) +
      geom_line() +
      scale_colour_discrete(name = "Modal age") +
      theme_bw() +
      labs(x = "Age error", y = "Frequency") +
      scale_y_continuous(labels = percent) +
      guides(col = guide_legend(nrow = 8))
      #theme(text = element_text(family="Calibri"))
}

# Plot relative bias of ages ##################################################

# Plot of the overall bias per modal age

plot_rb_ma <- function(rel_bias_tab) {
  rel_bias_tab %>%
    filter(`Modal age` != "Weighted Mean" & !is.na(all)) %>%
    select(`Modal age`, all) %>%
    rename(modal_age = `Modal age`) %>%
    mutate(modal_age=as.numeric(modal_age)) %>%
    ggplot(aes(x = modal_age, y = all, group = 1)) +
      geom_point(size = 2, colour = "#80B1D3") +
      geom_line(colour = "#80B1D3") +
      geom_abline(slope = 0, intercept = 0, colour = "#FB8072", lty = "dashed") +
      theme_bw() +
      labs(x = "Modal age", y = "Relative bias")
      #theme(text = element_text(family="Calibri"))
}


# Plot mean length at age by reader ###########################################

# For each reader and modal age plot mean length
plot_mla <- function(ad_long) {

  # x-axis text size if large number of readers
  text_x <- ifelse(length(unique(ad_long$reader)) > 20, 5, 8)

  ad_long %>%
    with(., tapply(length, list(age = age, reader = reader), mean, na.rm = TRUE)) %>%
    as.data.frame.table(responseName = "mean_len") %>%
    filter(!is.na(mean_len)) %>%
    ggplot(aes(x = factor(reader), y = mean_len, group = factor(age))) +
      geom_line(aes(color = factor(age))) +
      geom_point(aes(color = factor(age)), size = 2) +
      scale_color_discrete(name = "Age") +
      theme_bw() +
      labs(x = "Reader", y = "Mean length (mm)") +
      theme(axis.text.x = element_text(size = text_x, angle = 30, vjust = 0.9, hjust = 1.01)
            #text = element_text(family="Calibri")
            ) +
    guides(col = guide_legend(nrow = 8))
  
}

