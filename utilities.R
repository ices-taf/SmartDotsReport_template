
vname <- function(name) paste(name, group, sep = "_")
vsname <- function(name) paste(name, stratum, group, sep = "_")

Mode <- function(x) {
  as.numeric(names(sort(table(x), decreasing = TRUE)[1]))
}

ape <- function(x) {
  if (length(x) == 0) {
    return(numeric(0))
  }
  if (Mode(x) == 0) {
    NA
  } else {
    100 * mean(abs((x - mean(x, na.rm = TRUE)) / mean(x, na.rm = TRUE)), na.rm = TRUE)
  }
}

cv <- function (x) {
  if (length(x) == 0) {
    return(numeric(0))
  }
  if (Mode(x) == 0) {
    NA
  } else {
    sd(x, na.rm = TRUE) / mean(x, na.rm = TRUE) * 100
  }
}

capFirst <- function(s) {
    paste(toupper(substring(s, 1, 1)), substring(s, 2), sep = "")
}

