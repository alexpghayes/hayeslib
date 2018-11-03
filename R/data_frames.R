#' @importFrom magrittr %>%
#' @export
magrittr::`%>%`

#' Generic to calculate percentages in the same vein as `dplyr::count`
#'
#' @param x Object to summarize percentages for
#' @param ... Additional arguments
#'
#' @return Percentage summary of `x`
#' @export
perc <- function(x, ...) {
  UseMethod("perc")
}

#' @export
perc.numeric <- function(x, ...) {
  sum(x) / length(x)
}

#' @export
perc.logical <- function(x, ...) {
  sum(x) / length(x)
}

#' @export
#' @importFrom dplyr count mutate select
#' @importFrom rlang UQS quos
perc.default <- function(x, ...) {
  counts <- count(x, UQS(quos(...)))
  percs <- mutate(perc = n / sum(n))
  select(percs, -n)
}

#' Count the number of unique factor level combinations present in a data frame
#'
#' @param df Data frame
#' @param ... Bare/unquoted factors of interest
#'
#' @return Integer count of unique factor level combinations
#' @export
#' @importFrom dplyr distinct quos
#'
#' @examples
#' num_levels(iris, Species)
num_levels <- function(df, ...) {
  dim(dplyr::distinct(df, rlang::UQS(quos(...))))[1]
}

#' Transpose a dataframe
#'
#' Transpose a dataframe without accidentally converting everything to
#' character. Assumes first column of input is a vector of feature names.
#' If any of the data is non-numeric you're SOL and tranposed cols will
#' be character as well.
#'
#' @param df An unfortunately transposed data frame
#'
#' @return Transposed dataframe with appropriate column types
#' @export
t_df <- function(df) {
  vars <- dplyr::pull(df, 1)
  ids <- colnames(df)[-1]

  out <- dplyr::select(df, -1)
  out <- as_tibble(t(out))
  colnames(out) <- vars
  tibble::add_column(out, id = ids, .before = TRUE)
}

# Semi-general \code{unite} to vectorize a function across columns of dataframe
#
# Accepts columns from a dataframe and vectorizes/parallel maps a function
# across them, returning the result in a new column. Function must return a
# character vector because \code{purrr::pmap_char} enforces type-safety.
# combine <- function(df, col, ..., .f, remove = TRUE) {
#   merge_chrs <- select_vars(tbl_vars(df), UQS(quos(...)))
#   merge_syms <- syms(to_merge)
#   new_col <- enquo(col)
#   .f <- purrr::as_mapper(.f)
#
#   # TODO: make sure this works, type conversion, check as_mapper functionality
#
#   df <- mutate(df, quo_name(new_col) := simplify(pmap(list(!!!merge_syms), .f))
#   )
#   if (remove) df <- select(df, -one_of(merge_chrs))
#   df
# }


#' Summarize by group with less boilerplate
#'
#' @param df data frame
#' @param group_var bare/unquoted column name to group by
#' @param ... expressions as you would normally use in summarize
#'
#' @return ungrouped data frame containing results of grouped summary
#' @export
#'
summarize_by <- function(df, group_var, ...) {
  df <- dplyr::group_by(df, !!enquo(group_var))
  dplyr::summarize(df, !!!quos(...))
}


#' Mutate by group with less boilerplate
#'
#' @param df data frame
#' @param group_var bare/unquoted column name to group by
#' @param ... expressions as you would normally use in mutate
#'
#' @return ungrouped data frame containing results of grouped summary
#' @export
#'
mutate_by <- function(df, group_var, ...) {
  df <- dplyr::group_by(df, !!enquo(group_var))
  df <- dplyr::mutate(df, !!!quos(...))
  ungroup(df)
}


#' Select rows in the top n groups according to some summarization operation
#'
#' @param df data frame
#' @param group_var grouping variable
#' @param val_var value assessment variable to be summarized by group
#' @param val_fun value assessment function to apply to `val_var` by group
#'
#' @return data frame only with observations from top groups
#' @export
#' @importFrom dplyr enquo group_by summarize top_n inner_join
#' @importFrom rlang "!!"
#'
top_n_groups <- function(df, group_var, val_var, val_fun = sum) {
  group_quo <- enquo(group_var)
  df2 <- group_by(df, !!group_quo)
  df2 <- summarize(df2, group_total = val_fun(!!val_var))
  df2 <- top_n(df2, num_groups, group_total)
  inner_join(df, df2, !!group_quo)
}


#' Sample random groups from a dataframe with NSE
#'
#' Groups a dataframe by the specified feature, selects n of these groups and
#' discards the rest. Returned dataframe is ungrouped.
#'
#' @param df A data frame or data frame like object recognized by `dplyr`
#' @param group_var Feature to group by. Uses NSE like dplyr verbs.
#' @param n Number of groups to take. If there are fewer than `n` groups, uses the
#'
#' @return Dataframe with n random groups selected according to specified grouping feature.
#' @export
#' @importFrom dplyr distinct filter pull
#' @importFrom magrittr "%>%"
#'
#' @examples
#' sample_n_groups(mtcars, cyl, 2)
#'
sample_n_groups <- function(df, group_var, n) {
  group_var <- rlang::enquo(group_var)
  groups <- pull(distinct(df, !!group_var))

  if (n > length(groups)) {
    message("N greater than number of groups, returning original dataframe.")
    return(df)
  }
  filter(df, (!!group_var) %in% sample(groups, n))
}

#' Sample n features from a data frame randomly
#'
#' @param df Data frame to sample features from
#' @param n Number of columns to randomly select
#'
#' @return Sample subsample of features in a dataframe
#' @export
#'
#' @examples
#' sample_n_feats(mtcars, 3)
sample_n_feats <- function(df, n) {
  num_feats <- ncol(df)
  rand_feats <- sample(num_feats, min(n, num_feats))
  dplyr::select(df, !!rand_feats)
}


#' Spread, but keep the key column
#'
#' @param df data frame
#' @param key bare/unquoted key as in `spread`
#' @param value bare/unquoted value as in `spread`
#' @param ... additional arguments to `spread`
#'
#' @return spread data frame with key column retained
#' @export
#'
spread_keep <- function(df, key, value, ...) {
  key <- rlang::enquo(key)
  out <- dplyr::mutate(df, .dummy = !!key)
  out <- tidyr::spread(out, key = !!key, value = !!value, ...)
  dplyr::rename(out, !!key := .dummy)
}
