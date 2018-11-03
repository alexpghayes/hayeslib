#' Get an immediately shareable / pasteable link to data
#'
#' Uploads a data frame to Google Drive, make it shareable, and
#' copies the shareable link onto the clipboard. Requires that OAuth
#' has been set up. `googledrive` should help you through this if you
#' haven't set OAuth up already.
#'
#' Creates an intermediate local CSV that is deleted after upload is
#' complete.
#'
#' @param data A data frame, tibble or other object that be written to
#'   a CSV.
#' @param path What to call the data file, as a character. Can also be a
#'   file path for more precise placement of the data file. Path must
#'   exist both locally and in Google Drive (really, I'm not sure what
#'   happens when it doesn't).
#' @param direct If `TRUE`, the link can be used immediately in R to
#'   read in the file. If `FALSE`, the link is not directly to the CSV,
#'   but to the Google Drive preview of the file, which allows for nice
#'   previewing of the data.
#'
#' @return Invisibly returns the link as a character vector.
#' @export
#'
#' @examples
#'
#' # link for direct download / immediate use
#' lnk <- get_shareable_link_to_data(mtcars, "mtcars.csv")
#' mt <- read.csv(lk)
#' head(mt)
#'
#' # link to nice preview
#' get_shareable_link_to_data(mtcars, "mtcars.csv", direct = FALSE)
get_shareable_link_to_data <- function(data, path, direct = TRUE) {
  readr::write_csv(data, path)
  df <- googledrive::drive_upload(path, path)
  df <- googledrive::drive_share(df, role = "reader", type = "anyone")

  if (direct)
    link <- paste0("https://drive.google.com/uc?export=download&id=", df$id)
  else
    link <- googledrive::drive_link(df)

  clipr::write_clip(link)
  fs::file_delete(path)
  cat(link)
  invisible(link)
}