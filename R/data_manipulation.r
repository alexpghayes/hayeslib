top_n_groups <- function(df, group_var, num_groups) {
  .NotYetImplemented()

  df %>%
    group_by_(group_var) %>%
    summarise(group_total = sum(WordCount)) %>%
    top_n(num_groups, group_total) %>%
    inner_join(df, ., group_var)
}

rand_n_groups <- function(n) .NotYetImplemented()

complete_only <- function(df) df[complete.cases(df), ]

