## perform some sanity checks on the data

library(icesTAF)
library(jsonlite)

# load configuration
config <- read_json("config.json", simplifyVector = TRUE)

# get data from bootstrap folder  -------------------------------

ad <- read.taf("bootstrap/data.csv")
dist <- read.taf("bootstrap/dist.csv")

# some messages to the user ------

frmt_vector <- function(x) {
  paste(paste(names(x), ":", x), collapse = ", ")
}

check_ad <- function(ad, what = "ad") {
  checks <-
    list(
      c("Summary of ", what, " (", nrow(ad), " annotations in total):"),
      c("approved: ", sum(ad$IsApproved == "True"), ", unapproved: ", sum(ad$IsApproved == "False")),
      c("samples with no area: ", sum(ad$ices_area == "")),
      c("prep_method: ", frmt_vector(table(ad$prep_method)))
    )

  check_text <- paste(sapply(checks, paste, collapse = ""), collapse = "\n\t     * ")
  msg(check_text)
}

check_ad(ad)

check_ad(ad[ad$IsApproved == "True",], "ad (only approved)")


msg("Summary of ad (", nrow(ad), " annotations in total):\n",
    "\t\t approved: ", sum(ad$IsApproved == "True"), ", unapproved: ", sum(ad$IsApproved == "False"), "\n",
    "\t\t samples with no area: ", sum(ad$ices_area == ""), "\n",
    "\t\t prep_method: ", frmt_vector(table(ad$prep_method)), "\n")

msg("Summary of dist (", nrow(dist), " dots in total):\n",
    "\t\t approved: ", sum(dist$IsApproved == "True"), ", unapproved: ", sum(dist$IsApproved == "False"), "\n",
    "\t\t dots with no area: ", sum(dist$ices_area == ""), "\n",
    "\t\t dots with distance: ", sum(is.na(dist$pixelsPerMillimeter)), "\n")



