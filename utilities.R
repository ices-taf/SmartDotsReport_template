# Variuous small functions for simpler coding #################################

# na.rm versions of several function
meanNA <- function(x) mean(x, na.rm = TRUE)
sumNA <- function(x) sum(x, na.rm = TRUE)
sdNA <- function(x) sd(x, na.rm = TRUE)

#Rounding
round2 <- function(x) trunc(x + 0.5)

#Capitilizing first letter of string
capFirst <- function(s) {
  paste(toupper(substring(s, 1, 1)), substring(s, 2), sep = "")
}

# is.nan for data.frames
is.nan.data.frame <- function(x) {
  do.call(cbind, lapply(x, is.nan))
}

# supress NA in paste
paste3 <- function(...,sep=", ") {
  L <- list(...)
  L <- lapply(L,function(x) {x[is.na(x)] <- ""; x})
  ret <-gsub(paste0("(^",sep,"|",sep,"$)"),"",
             gsub(paste0(sep,sep),sep,
                  do.call(paste,c(L,list(sep=sep)))))
  is.na(ret) <- ret==""
  ret
}

# Return mode of list x
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}
