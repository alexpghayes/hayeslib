df <- tibble::tribble(
  ~a, ~b, ~c,
  1, NA, 5,
  2, 0, 4,
  NA, NA, 3
)

lzy_impute(df, a, b)

test_that("lzy_impute works with and without indication", {

  expect_warning(ind <- lzy_impute(df, a, b))
  expect_warning(no_ind <- lzy_impute(df, a, b, indicate = FALSE))

  expected_ind <- tibble::tribble(
    ~a, ~b, ~c, ~a_ind, ~b_ind,
    1, 0, 5, FALSE, TRUE,
    2, 0, 4, FALSE, FALSE,
    1.5, 0, 3, TRUE, TRUE
  )

  expect_equal(ind, expected_ind)
  expect_equal(no_ind, select(expected_ind, a:c))
})


test_that("simple na_counts", {

  expected <- tibble::tribble(
    ~a, ~b, ~c,
    1, 2, 0
  )

  expected <- mutate_all(expected, as.integer)

  expect_equal(na_counts(df), expected)
})


test_that("simple count_non_na", {
  x <- rep(NA, 3)
  y <- c(1, NA, 2)

  expect_equal(count_non_na(x), 0)
  expect_equal(count_non_na(y), 2)

})


test_that("simple remove_na", {
  x <- c(2, NA, 3, NaN)
  expect_equal(remove_na(x), c(2, 3))
})


test_that("simple to_na", {
  x <- c(1, NaN, 2, 3)
  expect_equal(to_na(x, 2, 3), c(1, NA, NA, NA))
})