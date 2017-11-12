#' @export
pdf_table <- function(df, caption = "TODO") {
  df %>%
    knitr::kable(format = "latex", booktabs = TRUE, caption = caption) %>%
    kableExtra::kable_styling(latex_options = c("hold_position", "striped"),
                              position = "center")
}