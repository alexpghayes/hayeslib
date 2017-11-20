# TODO: this all needs to be documented and tested. need to import dependencies!

#' @export
mcmc_trace <- function(postr) {
  postr %>%
    mutate(iter = row_number()) %>%
    gather(param, value, -iter) %>%
    ggplot(aes(iter, value)) +
    geom_line() +
    facet_wrap(~param, scales = "free_y") +
    labs(title = "MCMC Trace Plots", y = "Parameter Value", x = "Iteration")
}

#' @export
t_mcmc <- function(df) {
  df_t <- df %>% t() %>% as_tibble()
  tibble(param = colnames(df), samples = pmap(df_t, c))
}

#' @export
mcmc_stats <- function(df_mcmc) {
  mutate(df_mcmc,
         mean = map_dbl(samples, mean),
         hpd = map(samples, ~HPDinterval(as.mcmc(.x))),
         lo = map_dbl(hpd, 1),
         hi = map_dbl(hpd, 2)) %>%
    select(-hpd)
}

# extend to allow comparison of credible intervals?

#' @export
mcmc_hpd_int <- function(postr_stats) {
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
#' @export
rmvn_ql <- function(Q, ell) {
  p <- dim(Q)[1]
  ch_Q <- chol(Q)
  backsolve(ch_Q, forwardsolve(t(ch_Q), ell) + rnorm(p))
}