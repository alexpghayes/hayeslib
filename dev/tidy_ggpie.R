library(tidyverse)
library(rlang)

# wow it has been a long time since i wrote this
# don't think a hacky wrapper is the way to go anymore
# probably should move to a geom_pie as terrible as it is

# two input formats:
#   1. raw categorical data
#   2. nicely formatted counts

df <- starwars %>%
  group_by(species) %>%
  summarize(total_mass = sum(mass, na.rm = TRUE)) %>%
  mutate(labels = toupper(species))

ggplot(df, aes(x = species, y = total_mass)) +
  geom_pie()

theme_pie <- theme_gray() +
  theme(axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank())

ggpie2 <- function(df, main, labels = NULL, condition = NULL) {

  warning("Please do not use pie charts if you have any other option.",
          call. = FALSE)

  df <- group_by(df, !!!syms(c(main, condition))) %>%
    summarize(counts = n()) %>%
    mutate(perc = counts / sum(counts)) %>%
    arrange(desc(perc)) %>%
    mutate(label_pos = sum(perc) - cumsum(perc) + perc / 2,
           perc_text = paste0(round(perc * 100), "%"),
           main = forcats::fct_infreq(main))

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
    theme_pie()

  if (!is.null(condition)) p <- p + facet_wrap(condition)
  p
}


ggpie2(example, main = "resps", ex_labs) +
  labs(title = "unfacetted example")

example <- data.frame(
  resps = c("A", "A", "A", "F", "C", "C", "D", "D", "E"),
  cond = c(rep("cat A", 5), rep("cat B", 4))
)

ex_labs <- c("alpha", "charlie", "delta", "echo", "foxtrot")



ex_labs2 <- c("alpha", "charlie", "foxtrot", "delta", "charlie", "echo")

ggpie(example, main = "resps", labels = ex_labs2, condition = "cond") +
  labs(title = "facetted example")