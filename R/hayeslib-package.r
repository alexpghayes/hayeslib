#' hayeslib: a package to learn about packages. #meta
#'
#' Once the data manipulation functions get cleaned up there'll be a little explanation of what's available here.
#'
#' @docType package
#' @name hayeslib
NULL

.onAttach <- function(libname, pkgname) {
  packageStartupMessage("Welcome to Hayeslib.")
}