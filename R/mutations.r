reals_plz <- function(df, var) {
  var_quo <- enquo(var)
  filter(df, !is.na(!!var_quo) & !is.nan(!!var_quo))
}

to_na <- function(x, ...) {
  for (to_replace in list(...)) x[x == to_replace] <- NA
  x
}

yn_to <- function(x, as_type) as_type(tolower(x) == "yes")