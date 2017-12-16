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

#' @importFrom ggplot2 autoplot
#' @export
ggplot2::autoplot

#' Plot cross-validated elastic net paths
#'
#' Plots paths for features in the final model in color and provides a legend
#' for these features only. Features not in the final are plotted in grey and not
#' included in the legend.
#'
#' @param object Object of class \code{cv.glmnet}
#' @param rule Either "min" or "1-se". Specifies whether lambda should be
#' selected to minimize CV error, or to find the sparsest model within one standard
#' deviation of the minimum error. Default \code{cv.glmnet} behavior is to use the
#' one standard error rule, following Breiman.
#'
#' @return ggplot2 object with elastic net paths
#' @import ggplot2 dplyr
#' @importFrom tidyr gather
#' @export
#'
#' @examples
#'
#' library(glmnet)
#' data(Boston, package = "MASS")
#'
#' X <- model.matrix(medv ~ ., Boston)
#' y <- Boston$medv
#' cv_model <- cv.glmnet(X, y)
#'
#' autoplot(cv_model, rule = "min")
#'
autoplot.cv.glmnet <- function(object, rule = "1-se") {

  if (!inherits(object, "cv.glmnet"))
    stop("Only plots cv.glmnet objects.")

  if (rule == "min") {
    lam <- object$lambda.min
  } else if (rule == "1-se") {
    lam <- object$lambda.1se
  } else {
    stop("rule must be either 'min' or '1-se'.")
  }

  best_lam <- which(object$glmnet.fit$lambda == lam)

  selected_feats <- object$glmnet.fit$beta[, best_lam] %>%
    broom::tidy() %>%
    filter(x != 0) %>%
    pull(names)

  beta <- object$glmnet.fit$beta %>%
    as.matrix() %>%
    t() %>%
    tibble::as_tibble() %>%
    mutate(nll = -log(object$glmnet.fit$lambda))

  selected <- beta %>%
    select(one_of(selected_feats), nll) %>%
    gather(feat, value, -nll)

  not_selected <- beta %>%
    select(-one_of(selected_feats), nll) %>%
    gather(feat, value, -nll)

  ggplot() +
    geom_line(data = not_selected, aes(nll, value, group = feat), color = "grey") +
    geom_line(data = selected, aes(nll, value, color = feat)) +
    geom_vline(xintercept = -log(lam)) +
    labs(title = "Elastic net paths",
         subtitle = paste("Variables selected with", rule, "rule plotted in color"),
         x = expression(-log(lambda)),
         y = "Coefficient value") +
    theme_bw()
}
