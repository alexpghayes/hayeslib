# pass strings to functions

summarize_by <- function(df, group_vars, summ_var, summ_func = sum) {

  group_syms <- rlang::syms(group_vars)
  summ_sym <- rlang::sym(summ_var)

  df %>%
    group_by(!!!group_syms) %>%
    summarize(summ = summ_func(!!summ_sym)) %>%
    ungroup()
}

dat <- data.frame(cat = sample(c("A", "B"), 50, replace = TRUE),
                  value = rnorm(50))

dat %>%
  summarize_by("cat", "value")

# use NSE with functions

summarize_by <- function(df, group_var, summ_var, summ_func = sum) {

  group_quo <- enquo(group_var)
  summ_quo <- enquo(summ_var)

  df %>%
    group_by(!!group_quo) %>%
    summarize(summ = summ_func(!!summ_quo)) %>%
    ungroup()
}

# use nse outside a function

dat %>%
  summarize_by(cat, value)

summ_func <- sum

group_quo <- quo(cat)
summ_quo <- quo(value)

dat %>%
  group_by(!!group_quo) %>%
  summarize(summ = summ_func(!!summ_quo)) %>%
  ungroup


# use strings outside a function

group_var <- "cat"
summ_var <- "value"

group_syms <- rlang::syms(group_var)
summ_sym <- rlang::sym(summ_var)

dat %>%
  group_by(!!!group_syms) %>%
  summarize(summ = summ_func(!!summ_sym)) %>%
  ungroup()


### COMPLETE THIS LATER

top_n_groups <- function(df, group_var, num_groups) {
  df %>%
    group_by_(group_var) %>%
    summarize(group_total = sum(WordCount)) %>%
    top_n(num_groups, group_total) %>%
    inner_join(df, ., group_var)
}

