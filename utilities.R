
# potential new icesTAF functions

load.config <- function(file) {
  # load configuration json
  config <- jsonlite::read_json(file, simplifyVector = TRUE)
  lapply(names(config), function(x) assign(x, value = config[[x]], envir = .GlobalEnv))
  invisible(NULL)
}


# download raw data
# @param repo The name of the TAF repository to use as an input data set (output --> raw)
# @param ref Desired git reference. Could be a commit, tag, or branch name. Defaults to "master".
#
# # side effects
# * creates a folder called "raw" if one doesn't exist
# * checks to see if there is internet connection, if not, exits giving warning
# * checks to see if the current raw folder needs to be updated
# * if appropriate: downloads zip of raw folder and unzips, then deletes the zip.
download.raw <- function(repo, ref) {
  # for now just copy the local raw folder
  mkdir("raw")

  cp(paste0("../", repo ,"/output/data.csv"), "raw/data.csv")
  cp(paste0("../", repo ,"/output/dist.csv"), "raw/dist.csv")
  cp(paste0("../", repo ,"/output/exchange_style_8.docx"), "raw/exchange_style_8.docx")
}

