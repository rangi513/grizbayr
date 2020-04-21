context("Validate Input Column")

test_that("validate_input_column returns nothing with valid data", {
  input_df <- tibble::tibble(
    option_name = c("A", "B"),
    sum_clicks = c(1000, 1000),
    sum_conversions = c(100, 120)
  )
  expect_invisible(validate_input_column("option_name", input_df))
  expect_invisible(validate_input_column("sum_clicks", input_df))
  expect_invisible(validate_input_column("sum_conversions", input_df))
})

test_that("validate_input_column fails when required column is not in the df", {
  input_df <- tibble::tibble(
    option_name = c("A", "B"),
    sum_clicks = c(1000, 1000),
    sum_conversions = c(100, 120)
  )
  expect_error(validate_input_column("sum_revenue", input_df))
})

test_that("validate_input_column fails when option_name is not a character string column", {
  input_df <- tibble::tibble(
    option_name = as.double(c(0, 2)),
    sum_clicks = c(1000, 1000),
    sum_conversions = c(100, 120)
  )
  expect_error(validate_input_column("option_name", input_df))
})

test_that("validate_input_column fails when sum_clicks is not a double column", {
  input_df <- tibble::tibble(
    option_name = c("A", "B"),
    sum_clicks = c("something wrong", "another wrong thing"),
    sum_conversions = c(100, 120)
  )
  expect_error(validate_input_column("sum_clicks", input_df))
})

test_that("validate_input_column fails when a value is less than 0", {
  input_df <- tibble::tibble(
    option_name = as.double(c(0, 2)),
    sum_clicks = c(1000, 1000),
    sum_conversions = c(-2, 120)
  )
  expect_error(validate_input_column("sum_conversions", input_df))
})

test_that("validate_input_column returns nothing when a value is equal to 0", {
  input_df <- tibble::tibble(
    option_name = as.double(c(0, 2)),
    sum_clicks = c(1000, 0),
    sum_conversions = c(0, 120)
  )
  expect_invisible(validate_input_column("sum_conversions", input_df))
})
