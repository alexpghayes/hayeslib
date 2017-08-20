#' Plot the variable importance of a ranger object
#'
#' @param rf Object of class "ranger"
#'
#' @return \code{ggplot2} bar plot of variable importance
#' @export
#' @import ggplot2
var_plot <- function(rf) {
  stopifnot(inherits(rf, "ranger"))

  rf$variable.importance %>%
    tibble::enframe() %>%
    dplyr::arrange(desc(value)) %>%
    ggplot(aes(reorder(name, value), value)) +
    geom_bar(stat = "identity") +
    coord_flip() +
    labs(title = "Variable Importance",
         y = "Variable",
         x = "Relative Importance")
}


#' Create a pie chart in ggplot2
#'
#' Please only do this if forced to do so by a boss, etc.
#'
#' @param df data frame
#' @param main column name of response variable
#' @param labels column name with response labels
#' @param condition column name of grouping label
#'
#' @return ggplot2 object
#' @export
#' @import ggplot2 dplyr
#'
ggpie <- function(df, main, labels = NULL, condition = NULL) {

  warning("Please do not use pie charts if you have any other option.",
          call. = FALSE)

  df <- group_by_(df, .dots = c(condition, main)) %>%
    summarize(counts = n()) %>%
    mutate(perc = counts / sum(counts)) %>%
    arrange(desc(perc)) %>%
    mutate(label_pos = sum(perc) - cumsum(perc) + perc / 2,
           perc_text = paste0(round(perc * 100), "%"))

  # reorder the category factor levels to order the legend
  df[[main]] <- factor(df[[main]], levels = unique(df[[main]]))

  if (is.null(labels)) labels <- as.character(df[[main]])

  p <- ggplot(data = df, aes_string(x = factor(1), y = "perc", fill = main)) +
    geom_bar(stat = "identity", color = "black", width = 1) +
    geom_text(aes(x = 1.25, y = label_pos, label = perc_text), size = 4) +

    # add the category labels to the chart
    # increase x / play with label strings if labels aren't pretty
    geom_text(aes(x = 1.82, y = label_pos, label = labels), size = 4) +
    coord_polar(theta = "y") +
    scale_y_continuous(breaks = NULL) +
    scale_fill_discrete(name = "", labels = unique(labels)) +
    theme(text = element_text(size = 22),
          axis.ticks = element_blank(),
          axis.text = element_blank(),
          axis.title = element_blank())

  if (!is.null(condition)) p <- p + facet_wrap(condition)
  p
}
