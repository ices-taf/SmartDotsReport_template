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

# tag some feilds as missing?

# some messages to the user ------

frmt_vector <- function(x) {
  paste(paste0(names(x), ": ", x), collapse = ", ")
}

check_ad <- function(ad, what = "ad") {
  checks <-
    list(
      c("Summary of ", what),
      c("number of annotations: ", nrow(ad)),
      c("samples with missing area: ", sum(ad$ices_area == "")),
      c("samples with missing stock: ", sum(is.na(ad$stock) | ad$stock == "")),
      c("samples with missing prep_method: ", sum(is.na(ad$prep_method) | ad$prep_method == "")),
      c("prep_method names: ", frmt_vector(table(ad$prep_method))),
      c("Advanced reader annotations: ", sum(ad$expertise))
    )

  check_text <- paste(sapply(checks, paste, collapse = ""), collapse = "\n * ")

  # other checks
  multiple_annotations <-
    ad %>%
    dplyr::group_by(EventID, event_name, ices_area, SampleID, sample, FishID, reader) %>%
    dplyr::count() %>%
    dplyr::filter(n > 1) %>%
    dplyr::rename(annotations = n)

  if (nrow(multiple_annotations) > 0) {
    txt <- paste(capture.output(print(multiple_annotations)), collapse = "\n")
    image_urls <-
      sprintf(
        "https://smartdots.ices.dk/manage/viewDetailsImage?tblEventID=%i&SmartImageID=%i",
        multiple_annotations$EventID,
        multiple_annotations$sample)

    check_text <-
      paste0(check_text,
             "\n\n*****************\n",
               "**** Warning ****\n",
               "*****************\n\n",
             "Some readers have multiple annotations:\n\n",
             txt,
             "\n\nSee annotated images here:\n\t",
             paste(image_urls, collapse = "\n\t")
      )

  }


  if (sum(ad$expertise) == 0) {
    check_text <-
      paste0(check_text,
             "\n\n*****************\n",
               "**** Warning ****\n",
               "*****************\n\n",
             "** There are no advanced readers!                           **\n",
             "** the report scripts require there to be advanced readers. **"
      )

  }


  msg(check_text, "\n")
}

check_dist <- function(dist, what = "dist") {
  checks <-
    list(
      c("Summary of ", what),
      c("number of dots: ", nrow(dist)),
      c("dots with missing area: ", sum(dist$ices_area == "")),
      c("dots with missing distance: ", sum(is.na(dist$pixelsPerMillimeter)))
    )

  check_text <- paste(sapply(checks, paste, collapse = ""), collapse = "\n * ")
  msg(check_text, "\n")

  if (sum(!is.na(dist$pixelsPerMillimeter)) == 0) {
    check_text <-
      paste0("\n\n*****************\n",
               "**** Warning ****\n",
               "*****************\n\n",
             "** None of the images have a resolution!                       **\n",
             "** It is not possible to determine the distances between dots. **"
     )
    msg(check_text, "\n")
  }


}


if (config$onlyApproved == FALSE) {
  # check all data
  msg("Checking ALL data for Event: ", config$event_id)

  check_ad(ad, "ALL (approved and unapproved) annotations (sets of dots)")
  check_dist(dist, "ALL (approved and unapproved) dots")
}

msg("Checking approved data for Event: ", config$event_id)

check_ad(ad, "approved annotations (sets of dots)")
check_dist(dist[dist$IsApproved == 1,], "approved dots")

# done
