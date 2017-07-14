ggpie2 <- function(df, main, labels = NULL, condition = NULL) {

  warning("Please do not use pie charts if you have any other option.")

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

example <- data.frame(
  resps = c("A", "A", "A", "F", "C", "C", "D", "D", "E"),
  cond = c(rep("cat A", 5), rep("cat B", 4))
)

ex_labs <- c("alpha", "charlie", "delta", "echo", "foxtrot")

ggpie(example, main = "resps", labels) +
  labs(title = "unfacetted example")

ex_labs2 <- c("alpha", "charlie", "foxtrot", "delta", "charlie", "echo")

ggpie(example, main = "resps", labels = ex_labs2, condition = "cond") +
  labs(title = "facetted example")