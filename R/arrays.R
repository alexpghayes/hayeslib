#' Calculate the value of a quadratic form
#'
#' @param A Square n-by-n matrix
#' @param x Vector of length n
#'
#' @return Double value of the quadratic form x'Ax
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
#' @return Linearly spaced sequence from `start` to `stop`
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
#' In linear space the sequence starts at `base`^`start` and ends at
#' `base`^`stop`.
#'
#' @param start `base`^`start`is the starting value of the sequence.
#' @param stop `base`^`stop` is the final value in the sequence.
#' @param num Number of sequence elements to generate. Default is 15.
#' @param base The base of the log space. Default is 10.
#'
#' @return Sequence from `base`^`start` to `base`^`stop`
#'   evenly logspaced.
#' @export
#'
#' @examples
#' logspace(-5, 5)
#'
logspace <- function(start, stop, num = 15, base = 10) {
  base^seq(from = start, to = stop, length.out = num)
}

#' Create object of same shape and type as `x` filled with value `c`
#'
#' Again copying the Numpy zeros_like and ones_like interface
#'
#' @rdname constants_like
#'
#' @param x Object to imitiate
#' @param c Value to fill imitation with
#'
#' @return Object of same shape and type as `x` filled with value `c`
#' @export
constants_like <- function(x, c) {
  UseMethod("constants_like")
}

#' @export
constants_like.numeric <- function(x, c) {
  rep(c, length(x))
}

#' @export
constants_like.vector <- function(x, c) {
  rep(c, length(x))
}

#' @export
constants_like.matrix <- function(x, c) {
  array(c, dim(x))
}

#' @export
constants_like.array <- function(x, c) {
  array(c, dim(x))
}

#' @rdname constants_like
#' @export
ones_like <- function(x) {
  constants_like(x, c = 1)
}

#' @rdname constants_like
#' @export
zeros_like <- function(x) {
  constants_like(x, c = 0)
}

#' Create an identity matrix matching a vector
#'
#' @param x A vector
#'
#' @return An identity matrix with the same dimension as x.
#' @export
#'
#' @examples
#'
#' diag_like(1:2)
diag_like <- function(x) {
  stopifnot(is.vector(x))
  diag(length(x))
}

#' Check whether all elements of array or vector like objects are the same
#'
#' i.e. within a small but non-zero tolerance of each other
#'
#' @param x First array like object
#' @param y Second array like object
#' @param tol Acceptable difference between individual elements of `x`
#'   and `y`
#' @return logical indicating if values are elementwise within `tol`
#'   of each other
#' @export
allclose <- function(x, y, tol = 1e-12) {
  all(x - y < tol)
}

