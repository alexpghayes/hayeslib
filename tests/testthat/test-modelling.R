context("test-modelling.R")

test_that("compare_coefs gives results", {

  model <- lm(mpg ~ hp + drat, mtcars)
  other <- lm(mpg ~ hp * disp + drat, mtcars)

  hp_diff <- coef(model)["hp"] - coef(other)["hp"]
  drat_diff <- coef(model)["drat"] - coef(other)["drat"]

  comp <- compare_coefs(model, other, hp, drat)

  # check the difference in estimators is correct
  expect_equivalent(comp$difference, c(hp_diff, drat_diff))

  # check error out when trying to compare coefficients not in models
  expect_error(compare_coefs(model, other, noodles))
})
