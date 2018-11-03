#' Compare coefficients values across two different models
#'
#' @param model1 A model object (from `lm` for example) with a `tidy` method.
#' @param model2 A model object (from `lm` for example) with a `tidy` method.
#' @param ... Bare (unquoted) coefficient names present in both models.
#'
#' @return A tidy tibble with:
#'   * the coefficient names
#'   * the difference in coefficient values between models
#'   * standard error of the difference
#'   * z-statistic for the difference
#'   * p-value of the difference
#'   * lower 95 percent confidence bound for the difference
#'   * upper 95 percent confidence bound for the difference
#'   * the coefficient estimate from model1
#'   * the coefficient estimate from model2
#'   * the comparison method, always "Z-test"
#' @importFrom rlang quo_name quos
#' @importFrom purrr map_dfr
#' @importFrom broom tidy
#' @importFrom tibble tibble
#' @importFrom stats pnorm
#' @export
#'
#' @examples
#'
#' model <- lm(mpg ~ hp + drat, mtcars)
#' other <- lm(mpg ~ hp * disp + drat, mtcars)
#'
#' compare_coefs(model, other, hp, drat)
#'
compare_coefs <- function(model1, model2, ...) {
  compare_single_coef <- function(coef) {
    coef_name <- quo_name(coef)
    t1 <- dplyr::filter(tidy(model1), term == !!coef_name)
    t2 <- dplyr::filter(tidy(model2), term == !!coef_name)

    if (nrow(t1) == 0 || nrow(t2) == 0) {
      stop(paste0("Coefficient `", quo_name(coef),
                 "` not found in one or both models."), call. = FALSE)
    }

    diff <- t1$estimate - t2$estimate
    se <- sqrt(t1$std.error^2 + t2$std.error^2)
    statistic <- diff / se

    tibble(term = coef_name,
           difference = diff,
           std.error = se,
           statistic = statistic,
           p.value = 2 * pnorm(abs(statistic), lower.tail = FALSE),
           conf.low = diff - 1.96 * se,
           conf.high = diff + 1.96 * se,
           estimate1 = t1$estimate,
           estimate2 = t2$estimate,
           method = "Z-test")
  }
  coefs <- quos(...)
  map_dfr(coefs, compare_single_coef)
}
