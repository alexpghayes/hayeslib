library(dplyr)
library(ggplot2)

#
# df$main should contain observations of interest
# df$condition can optionally be used to facet wrap
#
# labels should be a character vector of same length as group_by(df, main) or
# group_by(df, condition, main) if facet wrapping
#

pie_chart <- function(df, main, labels = NULL, condition = NULL) {

  # convert the data into percentages. group by conditional variable if needed
  df <- group_by_(df, .dots = c(condition, main)) %>%
    summarize(counts = n()) %>%
    mutate(perc = counts / sum(counts)) %>%
    arrange(desc(perc)) %>%
    mutate(label_pos = cumsum(perc) - perc / 2,
           perc_text = paste0(round(perc * 100), "%"))

  # reorder the category factor levels to order the legend
  df[[main]] <- factor(df[[main]], levels = unique(df[[main]]))

  # if labels haven't been specified, use what's already there
  if (is.null(labels)) labels <- as.character(df[[main]])

  # actual plotting
  p <- ggplot(data = df, aes_string(x = factor(1), y = "perc", fill = main)) +

    # make stacked bar chart with black border
    geom_bar(stat = "identity", color = "black", width = 1) +

    # add the percents to the interior of the chart
    geom_text(aes(x = 1.25, y = label_pos, label = perc_text), size = 4) +

    # add the category labels to the chart
    # increase x / play with label strings if labels aren't pretty
    geom_text(aes(x = 1.82, y = label_pos, label = labels), size = 4) +

    # convert to polar coordinates
    coord_polar(theta = "y") +

    # formatting
    scale_y_continuous(breaks = NULL) +
    scale_fill_discrete(name = "", labels = unique(labels)) +
    theme(text = element_text(size = 22),
          axis.ticks = element_blank(),
          axis.text = element_blank(),
          axis.title = element_blank())

  # facet wrap if that's happening
  if (!is.null(condition)) p <- p + facet_wrap(condition)

  return(p)
}

# some example data
resps <- c("A", "A", "A", "F", "C", "C", "D", "D", "E")
cond <- c(rep("cat A", 5), rep("cat B", 4))
example <- data.frame(resps, cond)


ex_labs <- c("alpha", "charlie", "delta", "echo", "foxtrot")

pie_chart(example, main = "resps", labels = ex_labs) +
  labs(title = "unfacetted example")


ex_labs2 <- c("alpha", "charlie", "foxtrot", "delta", "charlie", "echo")

pie_chart(example, main = "resps", labels = ex_labs2, condition = "cond") +
  labs(title = "facetted example")
