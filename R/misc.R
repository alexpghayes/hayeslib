#' Create character vector without quotes
#'
#' Exactly the same as Hmisc::Cs except ain't nobody got time to import that.
#'
#' @param ... Bared, comma separated strings
#'
#' @return The bare code you wrote, but comma separated
#' @export
#'
#' @examples
#' lzy_chr(this, will, become, a, character, vector)
#'
lzy_chr <- function(...) {
  as.character(sys.call())[-1]
}

#' Get the RMSE of a set of predictions
#'
#' @param predicted Predicted values
#' @param true True values
#'
#' @return RMSE between true and predicted values
#' @export
#'
rmse <- function(predicted, true) {
  sqrt(mean((predicted - true)^2))
}