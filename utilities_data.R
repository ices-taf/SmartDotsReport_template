

# Determine modal age and CV ##################################################

# For each fish the cv and modal age are calculated.
# If one age is more frequent than others, that age is chosen as modal age.
# If no age is more frequent, then the average of all ages are chosen or
# if two (or more) ages are equally frequent then the age read by the most
# expericed reader will be chosen as modal age.
# WHich method to use is set in the ma_method variable.
# If the modal age is 0 the CV is set to 0 as well.

add_modal_trad <- function(ad, ma_method) {

  # ages by fish
  out <-
    ad %>%
    select(FishID, reader, age) %>%
    ddply(.(FishID, age), summarise, count = length(reader)) %>%
    spread(key = age, value = count)

  out[is.na(out)] <- 0

  ages <- out %>% select(-c(FishID))


  # Determine modal age depending on ma_method
  out$modal_trad <-
    if (ma_method == "Mean") {
      stop("mean not implemented yet")
    } else if (ma_method == "Mode") {
      apply(
        ages, 1,
        function(x) {
          if (!is.null(Mode_I(x))) {
            Mode_I(x)
          } else {
            trunc(mean(x, na.rm = TRUE) + 0.5)
          }
        }
      )
    }

  countcases <- vector(length = dim(out)[1])
  for (e in 1:dim(out)[1])
  {
    sel <- out[e, ]
    df <- sel[, -c(1, dim(out)[2])]
    max <- max(df)
    countcases[e] <- length(df[which(df == max)])
  }

  out$NModes_trad <- countcases

  # calculate CV
  out$cv <- apply(ages, 1, cv_I)
  out$cv[is.na(out$modal_age) | out$modal_age == 0] <- NA

  # merge CV and modal age to data
  right_join(ad, out, by = c("FishID"))
}



add_modal_linearweight <- function(ad, ma_method) {

  # ages by fish
  out <-
    ad %>%
    select(FishID, weight_I, age) %>%
    ddply(.(FishID, age), summarise, readerweight = sum(weight_I)) %>%
    spread(key = age, value = readerweight)

  out[is.na(out)] <- 0

  ages <- out %>% select(-c(FishID))

  # Determine modal age stage depending on ma_method
  out$modal_linearweight <-
    if (ma_method == "Mean") {
      stop("mean not implemented yet")
    } else if (ma_method == "Mode") {
      apply(
        ages, 1,
        function(x) {
          if (!is.null(Mode_I(x))) {
            Mode_I(x)
          } else {
            trunc(mean(x, na.rm = TRUE) + 0.5)
          }
        }
      )
    }

  countcases <- vector(length = dim(out)[1])
  for (e in 1:dim(out)[1])
  {
    sel <- out[e, ]
    df <- sel[, -c(1, dim(out)[2])]
    max <- max(df)
    countcases[e] <- length(df[which(df == max)])
  }

  out$NModes_linear <- countcases

  # merge CV and modal age to data
  right_join(ad, out, by = c("FishID"))
}




add_modal_negexpweight <- function(ad, ma_method) {

  # ages by fish
  out <-
    ad %>%
    select(FishID, weight_II, age) %>%
    ddply(.(FishID, age), summarise, readerweight = sum(weight_II)) %>%
    spread(key = age, value = readerweight)

  out[is.na(out)] <- 0

  ages <- out %>% select(-c(FishID))

  # Determine modal age stage depending on ma_method
  out$modal_negexpweight <-
    if (ma_method == "Mean") {
      stop("mean not implemented yet")
    } else if (ma_method == "Mode") {
      apply(
        ages, 1,
        function(x) {
          if (!is.null(Mode_I(x))) {
            Mode_I(x)
          } else {
            trunc(mean(x, na.rm = TRUE) + 0.5)
          }
        }
      )
    }

  countcases <- vector(length = dim(out)[1])
  for (e in 1:dim(out)[1])
  {
    sel <- out[e, ]
    df <- sel[, -c(1, dim(out)[2])]
    max <- max(df)
    countcases[e] <- length(df[which(df == max)])
  }

  out$NModes_negexp <- countcases

  # merge CV and modal age to data
  right_join(ad, out, by = c("FishID"))
}




select_mode <- function(ad, ma_method, mode_definition) {
  if (mode_definition == "multistage") {
    dat <- ad %>%
      select(FishID, modal_trad, NModes_trad, modal_linearweight, NModes_linear, modal_negexpweight, NModes_negexp) %>%
      distinct()

    dat$modal_age <-
      if (ma_method == "Mean") {
        stop("mean not implemented yet")
      } else if (ma_method == "Mode") {
        apply(
          dat, 1,
          function(x) {
            ifelse(x[3] == 1, as.numeric(x[2]), ifelse(x[5] == 1, as.numeric(x[4]), ifelse(x[7] == 1, as.numeric(x[6]), "Multimode")))
          }
        )
      }

    dat <- dat %>% select(FishID, modal_age)

    right_join(ad, dat, by = c("FishID"))
  } else {
    dat <- ad %>%
      select(FishID, modal_trad, NModes_trad) %>%
      distinct()

    dat$modal_age <-
      if (ma_method == "Mean") {
        stop("mean not implemented yet")
      } else if (ma_method == "Mode") {
        apply(
          dat, 1,
          function(x) {
            as.numeric(x[2])
          }
        )
      }

    dat <- dat %>% select(FishID, modal_age)

    right_join(ad, dat, by = c("FishID"))
  }
}



expertise_weight <- function(ad, expdat) {

  # Next the weight given to the current stock information and the general expertise
  weight_current_stock <- 1
  weight_general <- 0.25

  # Calculate the total number of otoliths read by each reader
  expdat$totalNumberOFftolitsReadCurrent <- expdat$noYearsCurrent * expdat$meanOtoCurrent

  # Create matrix with the maximum value for noYearsCurrent, meanOtoCurrent, totalNumberOfftolitsREadCurrent, and noYearsReading for each combination of stockCode, preparation_Method and iceS_Area. This maximum value will be used to calculate the relative expertise score.
  maxexp <- plyr::ddply(expdat, .(stockCode, preparation_Method, iceS_Area), summarise, maxnoYearsCurrent = max(noYearsCurrent), maxmeanOtoCurrent = max(meanOtoCurrent), maxtotalNumberOFftolitsReadCurrent = max(totalNumberOFftolitsReadCurrent), maxnoYearsReading = max(noYearsReading))

  expdat <- merge(expdat, maxexp, by = c("stockCode", "preparation_Method", "iceS_Area"))

  # Calculate the scores for the current stock (or combination of stock-icesarea-prepmethod)
  expdat$score_curr_stk <- ((expdat$noYearsCurrent / expdat$maxnoYearsCurrent) + (expdat$meanOtoCurrent / expdat$maxmeanOtoCurrent) + (expdat$totalNumberOFftolitsReadCurrent / expdat$maxtotalNumberOFftolitsReadCurrent)) * weight_current_stock
  # Calculate the scores for general expertise in age reading
  expdat$score_gral_stk <- ((expdat$noYearsReading / expdat$maxnoYearsReading)) * weight_general

  # decimals are round to 2 digits because it is considered that if the score is the same at that degree then the expertise ranking should be the same.
  expdat$tot_score <- round(expdat$score_curr_stk + expdat$score_gral_stk, digits = 2)

  # identify missing values, to be used later on during the production of the report, and save the expdat as csv file.
  expdat$iceS_Area[is.na(expdat$iceS_Area) | expdat$iceS_Area == " "] <- "missing"
  expdat$stockCode[is.na(expdat$stockCode) | expdat$stockCode == " "] <- "missing"
  expdat$preparation_Method[is.na(expdat$preparation_Method) | expdat$preparation_Method == " "] <- "missing"
  expdat$noYearsCurrent[is.na(expdat$noYearsCurrent) | expdat$noYearsCurrent == " "] <- "missing"
  expdat$meanOtoCurrent[is.na(expdat$meanOtoCurrent) | expdat$meanOtoCurrent == " "] <- "missing"
  expdat$totalNumberOFftolitsReadCurrent[is.na(expdat$totalNumberOFftolitsReadCurrent) | expdat$totalNumberOFftolitsReadCurrent == " "] <- "missing"
  expdat$noYearsReading[is.na(expdat$noYearsReading) | expdat$noYearsReading == " "] <- "missing"

  # Rank readers withing each stock-icesarea-prepmethod group based in their scores.
  rankdata <- plyr::ddply(expdat, .(stockCode, preparation_Method, iceS_Area), summarise, participantNumber = participantNumber, rank_readers = round(rank(tot_score), digits = 0), weight_I = (max(round(rank(tot_score), digits = 0)) + 1) - round(rank(tot_score), digits = 0))

  # Estimate the linear and exponential weights that will be used later on in the multistage approach
  rankdata$weight_II <- 1 / (1 + log(rankdata$rank_readers) + 0.0000000001)
  rankdata <- rankdata[with(rankdata, order(stockCode, preparation_Method, iceS_Area, rank_readers)), ]

  # Add the rank information to the expdat data
  expdat <- merge(expdat, rankdata, by = c("stockCode", "preparation_Method", "participantNumber", "iceS_Area"))

  # Save the completed expdat as csv
  write.csv(expdat, "data/reader_expertise_weighting_data.csv", row.names = FALSE)

  # Replace the reader_number with the estimated reader ranking, and add the linear and negative exponential weights (weight_I and weight_II)
  colnames(rankdata) <- c("stock", "prep_method", "ices_area", "reader_number", "rank_readers", "weight_I", "weight_II")
  ad <- merge(ad, rankdata, by = c("stock", "prep_method", "reader_number", "ices_area"), all.x = TRUE)
  ad$reader_number <- ad$rank_readers
  ad <- ad[, !colnames(ad) %in% c("rank_readers")]

  return(ad)
}
