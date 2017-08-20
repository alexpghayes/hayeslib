#' Mean impute and optionally add missingness indicator
#'
#' For fast prototyping only!
#'
#' @param df data frame
#' @param ... variable selections as per tidyselect
#' @param indicate logical, whether or not to add missingness indicate. this indicator is a bad idea because missingness will likely be correlated with your outcome and you'll think your model is great when you've just given it the answer to start with. see Harrell Regression Modelling Strategies for details.
#'
#' @return data frame with mean imputed data and possibly missingness indicators
#' @export
#' @importFrom rlang expr sym
#' @importFrom dplyr select_vars tbl_vars
#' @importFrom purrr map
#'
lzy_impute <- function(df, ..., indicate = TRUE) {

  warning("This function is loaded gun aimed at your foot.", call. = FALSE)

  dots <- quos(...)
  to_impute <- select_vars(tbl_vars(df), !!!dots)

  if (indicate) {
    ind_calls <- map(to_impute, ~expr(is.na(!!sym(.x))))
    names(ind_calls) <- paste0(to_impute, "_ind")
    df <- mutate(df, !!!ind_calls)
  }

  make_impute_call <- function(name) {
    name <- sym(name)
    expr(if_else(is.na(!!name), mean(!!name, na.rm = TRUE), !!name))
  }

  impute_calls <- map(to_impute, make_impute_call)
  names(impute_calls) <- to_impute

  mutate(df, !!!impute_calls)
}


#' Count non-NA values in a vector
#'
#' @param x vector
#'
#' @return number of non-NA values in the vector, 0 if there are none
#' @export
#'
count_non_na <- function(x) {
  if (all(is.na(x))) return(0)
  sum(!is.na(x))
}


#' Summarize missing data in each column of a dataframe
#'
#' @param df A dataframe
#'
#' @return A summary dataframe containing missing counts
#' @export
#'
na_counts <- function(df) {
  dplyr::summarise_all(df, ~sum(is.na(.x)))
}




#' Remove all NA values for a vector
#'
#' @param x vector
#'
#' @return vector with all NA values removed
#' @export
#'
#' @examples
#' remove_na(c(2, NA, 3, NaN))
remove_na <- function(x) {
  x[!is.na(x)]
}


#' Set an arbitrary number of elements in a vector to NA
#'
#' Will additionally set all NaN elements to NA.
#'
#' @param x A vector with elements that should set to NA
#' @param ... The elements that should be set to NA
#'
#' @return Vector with specified elements set to NA
#' @export
#'
#' @examples
#' to_na(c(1, NaN, 2, 3), 2, NaN)
#'
to_na <- function(x, ...) {
  for (to_replace in list(...)) x[x == to_replace] <- NA
  x[is.nan(x)] <- NA
  x
}