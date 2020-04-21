test_that("validate_input_df returns TRUE when df is valid for conversion rate", {
  input_df <- tibble::tibble(
    option_name = c("A", "B"),
    sum_clicks = c(1000, 1000),
    sum_conversions = c(100, 120)
  )
  expect_true(validate_input_df(input_df, "conversion_rate"))
})

test_that("validate_input_df fails when df is not valid for rev per session", {
  input_df <- tibble::tibble(
    option_name = c("A", "B"),
    sum_clicks = c(1000, 1000),
    sum_conversions = c(100, 120)
  )
  expect_error(validate_input_df(input_df, "rev_per_session"))
})

test_that("validate_input_df fails when df is not valid for rev per session", {
  input_df <- tibble::tibble(
    option_name = c("A", "B"),
    sum_clicks = c(1000, 1000),
    sum_conversions = c(100, 120)
  )
  expect_error(validate_input_df(input_df, "rev_per_session"))
})

test_that("validate_input_df fails when input is not a dataframe", {
  input_df <- list(
    option_name = c("A", "B"),
    sum_clicks = c(1000, 1000),
    sum_conversions = c(100, 120)
  )
  expect_error(validate_input_df(input_df, "conversion_rate"))
})

test_that("validate_input_df fails distribution name is invalid", {
  input_df <- list(
    option_name = c("A", "B"),
    sum_clicks = c(1000, 1000),
    sum_conversions = c(100, 120)
  )
  expect_error(validate_input_df(input_df, "invalid_distribution"))
})
