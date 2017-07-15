#' Create character vector without quotes
#'
#' Exactly the same as Hmisc::Cs except ain't nobody got time to import that.
#'
#' @param ...
#'
#' @return The bare code you wrote, but comma separated
#' @export
#'
#' @examples
#' lzy_chr(this, will, become, a, character, vector!)
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
rmse <- function(predicted, true) {
  sqrt(mean((predicted - true)^2))
}


#' Returns numbers evenly spaced in logspace.
#'
#' In linear space the sequence starts at base^start and ends at base^stop
#'
#' @param start base^start is the starting value of the sequence
#' @param stop base^stop is the final value in the sequence
#' @param num Number of sequence elements to generate. Default is 15.
#' @param base The base of the log space. Default is 10.
#'
#' @return Sequence from base^start and to base^stop evenly logspaced
#' @export
#'
#' @examples
#' logspace(-5, 5)
logspace <- function(start, stop, num = 15, base = 10) {
  base^seq(from = start, to = stop, length.out = num)
}
