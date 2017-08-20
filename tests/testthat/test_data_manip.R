# test_that("combine works", {
#   unite_at <- function(df, col, .vars, fun, ..., remove = TRUE) {
#     to_merge <- dplyr::select_vars(dplyr::tbl_vars(df),!!!.vars)
#     to_merge_syms <- rlang::syms(to_merge)
#     new_col <- enquo(col)
#
#     df <- mutate_at(df,
#                     .vars,
#                     quo_name(new_col) := simplify(pmap(list(
#                       !!!to_merge_syms
#                     ), fun, ...)))
#     if (remove)
#       df <- select(df,-one_of(to_merge))
#     df
#   }
#   unite_at(starwars, new, vars(matches("mass|height")), paste, sep = "_") %>%
#     select(name, new)
# })
