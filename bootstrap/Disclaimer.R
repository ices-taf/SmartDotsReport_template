#' ICES Disclaimer
#'
#' @name Disclaimer
#' @format csv files
#' @tafOriginator WGSMART
#' @tafYear 2020
#' @tafAccess Public
#' @tafSource script

ghurl <- "https://raw.githubusercontent.com"
repo <- "ices-tools-prod/disclaimers"
branch <- "master"

download(
  paste(
    ghurl,
    repo,
    branch,
    "Disclaimer_SmartDots.txt",
    sep = "/"
  ),
  destfile = "Disclaimer.txt"
)
