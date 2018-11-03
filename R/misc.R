#' @export
yell <- function() {
  notifier::notify(
    title = "R Command Has Completed",
    msg = "Time to get off Twitter"
  )
}

#' @export
blam <- function(object) {
  name <- substitute(object)
  readr::write_rds(object, paste0(name, ".rds"))
}

#' @export
unblam <- function(object) {
  name <- substitute(object)
  readr::read_rds(paste0(name, ".rds"))
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


#' Get OLS regression coefficients
#'
#' Assumes data matrix is tall or square, but not wide. Does not add
#'   intercept.
#'
#' @param X Data matrix (`n` by `p`)
#' @param y Response vector (length `n`)
#'
#' @return Vector of regression coefficients
#' @export
beta_ols <- function(X, y) {
  as.numeric(solve(crossprod(X), crossprod(X, y)))
}
