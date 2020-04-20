context("Update Gamma")

test_that("update_gamma updates with priors", {
  expected_output <- list(k = 3, theta = 1000/(1 + 1000 * 100))
  output <- update_gamma(k = 1, theta = 100, priors = list(k0 = 2, theta0 = 1000))
  testthat::expect_equal(output, expected_output)
})

test_that("update_gamma updates without priors", {
  expected_output <- list(k = 11, theta = 250/(1 + 250 * 200))
  output <- update_gamma(k = 10, theta = 200)
  testthat::expect_equal(output, expected_output)
})

test_that("update_gamma updates but warns with invalid priors", {
  expected_output <- list(k = 11, theta = 250/(1 + 250 * 200))
  invalid_priors <- list(k0 = -2, theta0 = 1000)
  testthat::expect_warning({
    output <- update_gamma(k = 10, theta = 200, priors = invalid_priors)
  })
  testthat::expect_equal(output, expected_output)
})
