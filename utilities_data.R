
library(plyr)

# Determine modal age and CV ##################################################

# For each sample the cv and modal age are calculated.
# If one age is more frequent than others, that age is chosen as modal age.
# If no age is more frequent, then the average of all ages are chosen or
# if two (or more) ages are equally frequent then the age read by the most
# expericed reader will be chosen as modal age.
# WHich method to use is set in the ma_method variable.
# If the modal age is 0 the CV is set to 0 as well.

add_modalage <- function(dat, ma_method) {

  # Determine modal age and calculate cv depending on ma_method
  if (ma_method == "Mean") {
    ma_cv <-
      ddply(dat[!is.na(dat$age),], .(sample),
            function(x) {
              data.frame(cv = round2(sd(x$age, na.rm = TRUE)/(mean(x$age, na.rm = TRUE))*100),
                         modal_age = ifelse(
                                       length(which(table(x$age) == max(table(x$age)))) < 2,
                                          Mode(x$age),
                                          round2(mean(x$age, na.rm = TRUE)))
                      )
            })
  } else if (ma_method == "Mode") {
    ma_cv <-
      ddply(dat[!is.na(dat$age),], .(sample),
            function(x) {
              data.frame(cv = round2(sd(x$age, na.rm = TRUE)/(mean(x$age, na.rm = TRUE))*100),
                         modal_age = ifelse(
                                       !is.null(Mode(x$age)),
                                         Mode(x$age),
                                         round2(mean(x$age, na.rm = TRUE)))
                      )
            })
  }

  # If modal as is NA or 0 then CV is set to NA
  ma_cv$cv[is.na(ma_cv$modal_age) | ma_cv$modal_age == 0] <- NA

  # merge CV and modal age to data
  dplyr::right_join(dat, ma_cv, by = "sample")
}

