context("Estimate Value Remaining")


test_that("estimate_value_remaining returns single value around 0.13", {
  input_df <- tibble::tibble(
    option_name = c("A", "B", "C"),
    sum_clicks = c(1000, 1000, 1000),
    sum_conversions = c(100, 120, 110)
  )
  output <- estimate_value_remaining(input_df, distribution = "conversion_rate")
  expect_length(output, 1)
  expect_true(is.double(output))
  expect_lt(output, 0.18)
  expect_gt(output, 0.10)
})

test_that("estimate_lift_vs_baseline returns single value around .22 when threshold is 0.99", {
  input_df <- tibble::tibble(
    option_name = c("A", "B", "C"),
    sum_clicks = c(1000, 1000, 1000),
    sum_conversions = c(100, 120, 110)
  )
  output <- estimate_value_remaining(input_df, distribution = "conversion_rate", threshold = 0.99)
  expect_length(output, 1)
  expect_true(is.double(output))
  expect_lt(output, 0.26)
  expect_gt(output, 0.15)
})
