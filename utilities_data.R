
#Rounding
Mode <- function(x) {
  as.numeric(names(sort(table(x), decreasing = TRUE)[1]))
}

cv <- function (x) {
  sd(x, na.rm = TRUE) / mean(x, na.rm = TRUE) * 100
}

# Determine modal age and CV ##################################################

# For each sample the cv and modal age are calculated.
# If one age is more frequent than others, that age is chosen as modal age.
# If no age is more frequent, then the average of all ages are chosen or
# if two (or more) ages are equally frequent then the age read by the most
# expericed reader will be chosen as modal age.
# WHich method to use is set in the ma_method variable.
# If the modal age is 0 the CV is set to 0 as well.

add_modalage <- function(ad, ma_method) {

  # ages by fish
  out <-
    ad %>%
    select(FishID, reader, age) %>%
    spread(key = reader, value = age)

  ages <- out %>% select(-FishID)

  # Determine modal age depending on ma_method
  out$modal_age <-
    if (ma_method == "Mean") {
      stop ("mean not implemented yet")
    } else if (ma_method == "Mode") {
      apply(ages, 1,
            function(x) {
              if (!is.null(Mode(x))) {
                Mode(x)
              } else {
                trunc(mean(x, na.rm = TRUE) + 0.5)
              }
            })
    }

  # calculate CV
  out$cv <- apply(ages, 1, cv)
  out$cv[is.na(out$modal_age) | out$modal_age == 0] <- NA

  # merge CV and modal age to data
  right_join(ad, out, by = "FishID")
}
