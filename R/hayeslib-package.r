#' hayeslib: a package to learn about packages. #meta
#'
#' This packages provides functions in R/ and data analyses in vignettes/
#'
#' @section hayeslib functions:
#' oh look this is how a section works
#'
#' @docType package
#' @name hayeslib
NULL

.onAttach <- function(libname, pkgname) {
  packageStartupMessage("Welcome to Hayeslib.")
}