test_that("linspace works", {

  linspace_out <- linspace(-5, 5, num = 10)
  seq_out <- seq(-5, 5, length.out = 10)

  expect_equal(linspace_out, seq_out)
})



test_that("logspace agrees with numpy", {

  numpy_res <- c(1.00000000e-05,   5.17947468e-05,   2.68269580e-04,
                 1.38949549e-03,   7.19685673e-03,   3.72759372e-02,
                 1.93069773e-01,   1.00000000e+00,   5.17947468e+00,
                 2.68269580e+01,   1.38949549e+02,   7.19685673e+02,
                 3.72759372e+03,   1.93069773e+04,   1.00000000e+05)

  expect_equal(logspace(-5, 5), numpy_res)
})


test_that("lzy_chr works", {

  lzy_out <- lzy_chr(this, is, a, char, vec)
  c_out <- c("this", "is", "a", "char", "vec")

  expect_equal(lzy_out, c_out)
})


test_that("rmse ain't cray", {

  x <- 1:3
  y <- 4:6

  expect_equal(rmse(x, y), sqrt(mean(x - y)^2))
})



