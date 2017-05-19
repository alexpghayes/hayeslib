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
#' @examples logspace(-5, 5)
logspace <- function(start, stop, num = 15, base = 10) {
  base^seq(from = start, to = stop, length.out = num)
}

#' Returns the sequence of indices for a vector or list object.
#'
#' @param x Vector or list
#'
#' @return Indices for vector or list input. Otherwise throws an error.
#' @export
#'
#' @examples
#' indices(c("mouse", "cat", "dog"))
#'
indices <- function(x) {
  if (!is.vector(x)) stop("Must input a vector")
  1:length(x)
}