# TODO: test and document

#' @export
quad_form <- function(A, x) {
  colSums(x * (A %*% x))
}

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

#' @export
zeros_like <- function(x) {
  array(0, dim(x))
}

#' @export
ones_like <- function(x) {
  array(1, dim(x))
}

#' @export
allclose <- function(x, y, tol = 1e-12) {
  all(x - y < tol)
}

