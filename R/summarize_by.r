top_n_groups <- function(df, group_var, num_groups) {
  df %>%
    group_by_(group_var) %>%
    summarise(group_total = sum(WordCount)) %>%
    top_n(num_groups, group_total) %>%
    inner_join(df, ., group_var)
}
