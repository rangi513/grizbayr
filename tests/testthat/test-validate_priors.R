context("Validate Priors")

test_that("validate_priors selects subset of valid priors", {
  expected_output <- list(b = 2, c = 3)
  priors <- list(a = 1, b = 2, c = 3)
  valid_priors <- c("b", "c")
  default_priors <- list(b = 1, c = 1)
  output <- validate_priors(priors, valid_priors, default_priors)
  testthat::expect_equal(output, expected_output)
})

test_that("validate_priors returns default priors when a single parameter is invalid", {
  priors <- list(a = 1, b = 2, c = -4)
  valid_priors <- c("b", "c")
  default_priors <- list(b = 1, c = 1)
  testthat::expect_warning({
    output <- validate_priors(priors, valid_priors, default_priors)
  })
  testthat::expect_equal(output, default_priors)
})
