context("Sequence generators")

numpy_res <- c(1.00000000e-05,   5.17947468e-05,   2.68269580e-04,
               1.38949549e-03,   7.19685673e-03,   3.72759372e-02,
               1.93069773e-01,   1.00000000e+00,   5.17947468e+00,
               2.68269580e+01,   1.38949549e+02,   7.19685673e+02,
               3.72759372e+03,   1.93069773e+04,   1.00000000e+05)

test_that("logspace agrees with numpy.logspace(-5, 5, num=15)", {
  expect_equal(logspace(-5, 5), numpy_res)
})

