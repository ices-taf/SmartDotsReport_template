## perform some sanity checks on the data

library(icesTAF)
library(jsonlite)
library(dplyr)
library(tidyr)

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

  # other checks
  multiple_annotations <-
    ad %>%
    group_by(FishID, reader) %>%
    count() %>%
    filter(n > 1) %>%
    rename(annotations = n)
  if (nrow(multiple_annotations) > 0) {
    txt <- paste(capture.output(print(multiple_annotations)), collapse = "\n")
    msg("Some readers have multiple annotations:\n", txt)
  }

}

check_dist <- function(dist, what = "dist") {
  checks <-
    list(
      c("Summary of dist (", nrow(dist), " dots in total):"),
      c("approved: ", sum(dist$IsApproved == "True"), ", unapproved: ", sum(dist$IsApproved == "False")),
      c("dots with no area: ", sum(dist$ices_area == "")),
      c("dots with distance: ", sum(is.na(dist$pixelsPerMillimeter)))
    )

  check_text <- paste(sapply(checks, paste, collapse = ""), collapse = "\n\t     * ")
  msg(check_text)
}


msg("Checking data for Event: ", config$event_id)

check_ad(ad)
check_ad(ad[ad$IsApproved == "True",], "ad (only approved)")
check_dist(dist)
check_dist(dist[dist$IsApproved == "True",], "dist (only approved)")



# done
