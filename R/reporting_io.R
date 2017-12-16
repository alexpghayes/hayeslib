#' Add a nicely formatted table to a PDF knitr from R markdown
#'
#' @param df Data frame to print as a nice table
#' @param title Title of table
#'
#' @return Kable something?
#' @export
#'
pdf_table <- function(df, title = "TODO: ADD TITLE") {
  out <- knitr::kable(df, format = "latex", booktabs = TRUE, caption = title)
  kableExtra::kable_styling(out, latex_options = c("HOLD_position", "striped"),
                            position = "center")
}
#
# read_tcsv <- function(file, header = TRUE, sep = ",", ...) {
#
#   num_feats <- max(count.fields(file, sep = sep), na.rm = TRUE)
#   lines <- readLines(file)
#
#   .splitvar <- function(x, sep, n) {
#     var <- unlist(strsplit(x, split=sep))
#     length(var) <- n
#     var
#   }
#
#   chr_array <- do.call(cbind, lapply(lines, .splitvar, sep = sep, n = num_feats))
#   t_lines <- apply(chr_array, 1, paste, collapse = sep)
#   readr::read_csv(t_lines, sep = sep)
# }
