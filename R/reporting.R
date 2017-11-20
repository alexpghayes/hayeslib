#' Add a nicely formatted table to a PDF knitr from R markdown
#'
#' @param df Data frame to print as a nice table
#' @param title Title of table
#'
#' @return Kable something?
#' @export
#'
pdf_table <- function(df, title = "TODO: ADD TITLE") {
  out <- knitr::kable(df, format = "latex", booktabs = TRUE, caption = caption)
  kableExtra::kable_styling(out, latex_options = c("HOLD_position", "striped"),
                            position = "center")
}