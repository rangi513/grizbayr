context("Is Prior Valid")

test_that("is_prior_valid returns TRUE when valid value exists.", {
  example_prior_list <- list(x = 1, y = 10, z = 15)
  output <- is_prior_valid(priors_list = example_prior_list, valid_prior = "y")
  testthat::expect_equal(output, expected = TRUE)
})

test_that("is_prior_valid returns TRUE when valid value exists and list is length 1.", {
  example_prior_list <- list(y = 10)
  output <- is_prior_valid(priors_list = example_prior_list, valid_prior = "y")
  testthat::expect_equal(output, expected = TRUE)
})

test_that("is_prior_valid returns FALSE when empty list is passed in.", {
  example_prior_list <- list()
  output <- is_prior_valid(priors_list = example_prior_list, valid_prior = "y")
  testthat::expect_equal(output, expected = FALSE)
})

test_that("is_prior_valid returns FALSE when valid value is not in list.", {
  example_prior_list <- list(x = 1, y = 10, z = 15)
  expect_warning({
    output <- is_prior_valid(priors_list = example_prior_list, valid_prior = "a")
  })
  testthat::expect_equal(output, expected = FALSE)
})

test_that("is_prior_valid returns FALSE when negative value is in list.", {
  example_prior_list <- list(x = 1, y = -8, z = 15)
  expect_warning({
    output <- is_prior_valid(priors_list = example_prior_list, valid_prior = "y")
  })
  testthat::expect_equal(output, expected = FALSE)
})

test_that("is_prior_valid returns FALSE value in list is 0.", {
  example_prior_list <- list(x = 1, y = 0, z = 15)
  expect_warning({
    output <- is_prior_valid(priors_list = example_prior_list, valid_prior = "y")
  })
  testthat::expect_equal(output, expected = FALSE)
})
