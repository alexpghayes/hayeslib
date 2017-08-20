#' Returns evenly spaced numbers.
#'
#' This wraps seq trivially. What can I say, I like the Numpy API.
#'
#' @param start base^start is the starting value of the sequence
#' @param stop base^stop is the final value in the sequence
#' @param num Number of sequence elements to generate. Default is 15.
#'
#' @return Linearly spaced sequence from \code{start} to \code{stop}
#' @export
#'
#' @examples
#' linspace(-5, 5)
#'
linspace <- function(start, stop, num = 15) {
  seq(from = start, to = stop, length.out = num)
}

#' Returns numbers evenly spaced in logspace.
#'
#' In linear space the sequence starts at \code{base}^\code{start} and ends at
#' \code{base}^\code{stop}.
#'
#' @param start \code{base}^\code{start}is the starting value of the sequence.
#' @param stop \code{base}^\code{stop} is the final value in the sequence.
#' @param num Number of sequence elements to generate. Default is 15.
#' @param base The base of the log space. Default is 10.
#'
#' @return Sequence from \code{base}^\code{start} to \code{base}^\code{stop}
#'   evenly logspaced.
#' @export
#'
#' @examples
#' logspace(-5, 5)
#'
logspace <- function(start, stop, num = 15, base = 10) {
  base^seq(from = start, to = stop, length.out = num)
}

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