context("Validate Data Values")

test_that("validate_data_values returns same list of 2 when values are valid.", {
  data_values <-  list(successes = 22, failures = 100)
  output <- validate_data_values(data_values)
  testthat::expect_equal(output, expected = data_values)
})

test_that("validate_data_values returns same list of 4 when values are valid.", {
  data_values <-  list(successes = 22, failures = 100, revenue = 5678, cost = 1234)
  output <- validate_data_values(data_values)
  testthat::expect_equal(output, expected = data_values)
})

test_that("validate_data_values fails when list is empty.", {
  empty_data_values <-  list()
  testthat::expect_error(validate_data_values(empty_data_values))
})

test_that("validate_data_values fails when one value is less than 0.", {
  data_values <-  list(successes = -10, failures = 100)
  testthat::expect_error(validate_data_values(data_values))
})

test_that("validate_data_values returns the same list when one value equals 0.", {
  data_values <-  list(successes = 0, failures = 100)
  output <- validate_data_values(data_values)
  testthat::expect_equal(output, expected = data_values)
})
