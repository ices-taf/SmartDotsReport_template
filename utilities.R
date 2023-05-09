
vname <- function(name) paste(name, group, sep = "_")
vsname <- function(name) paste(name, stratum, group, sep = "_")


# Two different ways of calculating the Mode are needed, one for weighted mode and the other for other calculations where the meaning of the column is different.
# This first function to estimate the Mode is created to deal with matrices were the columns are the age, and the content of the cell the number of readers that decided each age. This function Mode_I goes with the function cv_I
Mode_I <- function(x) {
  as.numeric(names(sort(x, decreasing = TRUE)[1]))
}

# This second function to estimate the Mode is created to deal with matrices were the columns are the readers, and the content of the cell the age decided by each reader. This function Mode_II goes with the function cv_II
Mode_II <- function(x) {
  as.numeric(names(sort(table(x), decreasing = TRUE)[1]))
}


# Since the mean(x) is dividing, if it is zero, or very close to zero, the result is going to be a very high number, which will be difficult to interpret. For example, in the case of otoliths for which the modal age was zero, a reader that almost always decided that the age was zero (agreed with the mode) excepting one or two cases will obtain a very high ape, because the mean of his annotations was very close to zero; while other reader that was not so on agreement with the mode zero, will obtain a lower ape. 
ape <- function(x) {
  if (length(x) == 0) {
    return(numeric(0))
  }
  if (Mode_II(x) == 0) {
    NA
  } else {
    100 * mean(abs((x - mean(x, na.rm = TRUE)) / mean(x, na.rm = TRUE)), na.rm = TRUE)
    }
}


# This first function to estimate the cv is created to deal with matrices were the columns are the age, and the content of the cell the number of readers that decided each age. This function cv_I goes with the function Mode_I, that estimates the mode for matrices equally designed (i.e. columns are the age, and the content of the cell the number of readers that decided each age, while rows are the fishID)
cv_I <- function (x) {
  if (length(x) == 0) {
    return(numeric(0))
  }

  if(Mode_I(x) == 0) {
    NA
    } else {
      dat=rep.int(as.numeric(names(x[which(x>0)])),times=x[which(x>0)])
      sd(dat, na.rm = TRUE) / mean(dat, na.rm = TRUE) * 100}
  
}

# This second function to estimate the cv is created to deal with matrices were the columns are the readers, and the content of the cell the age decided by each reader. This function cv_II goes with the function Mode_II, that estimates the mode for matrices equally designed (i.e. columns are readers and content is the age decided by each reader, while rows are the fishID)
cv_II <- function (x) {
  if (length(x) == 0) {
    return(numeric(0))
  }
  
  if(Mode_II(x) == 0) {
    NA
  } else {
    sd(x, na.rm = TRUE) / mean(x, na.rm = TRUE) * 100}
  
}


capFirst <- function(s) {
    paste(toupper(substring(s, 1, 1)), substring(s, 2), sep = "")
}

