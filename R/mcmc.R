#' Create a trace plot from a dataframe of posterior samples
#'
#' @param postr Data frame of posterior samples
#' @return ggplot object
#' @import ggplot2 dplyr
#'
#' @export
mcmc_trace <- function(postr) {
  postr %>%
    mutate(iter = row_number()) %>%
    tidyr::gather(param, value, -iter) %>%
    ggplot(aes(iter, value)) +
    geom_line() +
    facet_wrap(~param, scales = "free_y") +
    labs(title = "MCMC Trace Plots", y = "Parameter Value", x = "Iteration")
}

#' Collect posterior samples into a list column
#'
#' @param df Data frame of posterior samples where each row corresponds to a sample
#' @return Data frame with two columns: one containing parameter names, one containing
#'   a vector of parameter samples
#' @import tibble
#' @importFrom purrr pmap
#'
#' @export
t_mcmc <- function(df) {
  df_t <- as_tibble(t(df))
  tibble(param = colnames(df), samples = pmap(df_t, c))
}

#' Compute common statistics on posterior samples
#'
#' @param df_mcmc A data frame with posterior samples in a list column called
#'   \code{samples}
#'
#' @export
#' @importFrom dplyr mutate select
#' @importFrom purrr map map_dbl
mcmc_stats <- function(df_mcmc) {
  mutate(df_mcmc,
         mean = map_dbl(samples, mean),
         hpd = map(samples, ~coda::HPDinterval(coda::as.mcmc(.x))),
         lo = map_dbl(hpd, 1),
         hi = map_dbl(hpd, 2)) %>%
    select(-hpd)
}



#' Plot 95 percent HPD credible intervals for posterior samples
#'
#' If the posterior sample data frame has a column \code{mle}, the column is
#' assumed to MLE point estimates of parameters and these are plotted next to
#' the credible intervals as red triangles.
#'
#' @param postr_stats Data frame returned from \code{\link{mcmc_stats}}
#' @return ggplot object
#'
#' @export
#' @import ggplot2
mcmc_hpd_int <- function(postr_stats) {

  # TODO: extend to allow comparison of credible intervals?

  p <- ggplot(postr_stats) +
    geom_pointrange(aes(param, mean, ymin = lo, ymax = hi)) +
    labs(title = "Parameter estimates with 95 HPD interval",
         y = "Value", x = "Parameter")

  if ("mle" %in% colnames(postr_stats)) {
    p <- p + geom_point(aes(param, mle), color = "red", shape = 2)
  }
  p
}

#' Sample efficiently from the MVN parameterized by Q matrix and ell vector
#'
#' i.e draw a single sample from \deqn{MVN(\ell Q^{-1}, Q^{-1})}
#'
#' @param Q Precision matrix for MVN distribution
#' @param ell Multiplicative factor combined with \code{Q} to get mean of MVN dist
#'
#' @export
rmvn_ql <- function(Q, ell) {
  p <- dim(Q)[1]
  ch_Q <- chol(Q)
  backsolve(ch_Q, forwardsolve(t(ch_Q), ell) + stats::rnorm(p))
}