context("Estimate All Values")

test_that("Estimate All Values returns correct types", {
  expected_output <-
  input_df <- tibble::tibble(
    option_name = c("A", "B", "C"),
    sum_clicks = c(1000, 1000, 1000),
    sum_conversions = c(100, 120, 110)
  )
  output <- estimate_all_values(input_df, distribution = "conversion_rate", wrt_option_lift = "A")
  expect_length(output, 4)

  is_wp_output_tibble <- is.data.frame(output[["Win Probability"]])
  expect_true(is_wp_output_tibble)

  is_wpb_output_tibble <- is.data.frame(output[["Win Probability vs Baseline"]])
  expect_true(is_wpb_output_tibble)

  is_vr_double <- is.double(output[["Value Remaining"]])
  expect_true(is_vr_double)

  is_lift_double <- is.double(output[["Lift vs Baseline"]])
  expect_true(is_lift_double)
})
