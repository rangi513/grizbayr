context("Estimate Lift Vs Baseline")

test_that("estimate_lift_vs_baseline returns single value around .12", {
  input_df <- tibble::tibble(
    option_name = c("A", "B", "C"),
    sum_clicks = c(1000, 1000, 1000),
    sum_conversions = c(100, 120, 110)
  )
  output <- estimate_lift_vs_baseline(input_df,
                                      distribution = "conversion_rate",
                                      wrt_option = "A",
                                      metric = "lift",
                                      threshold = 0.7)
  expect_length(output, 1)
  expect_lt(output, 0.18)
  expect_gt(output, 0.06)
})

test_that("estimate_lift_vs_baseline returns single value around .20 when threshold is 0.5", {
  input_df <- tibble::tibble(
    option_name = c("A", "B", "C"),
    sum_clicks = c(1000, 1000, 1000),
    sum_conversions = c(100, 120, 110)
  )
  output <- estimate_lift_vs_baseline(input_df,
                                      distribution = "conversion_rate",
                                      wrt_option = "A",
                                      metric = "lift",
                                      threshold = 0.5)
  expect_length(output, 1)
  expect_lt(output, 0.25)
  expect_gt(output, 0.15)
})
