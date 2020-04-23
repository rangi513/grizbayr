context("Estimate Win Probability")

test_that("estimate_win_prob returns tibble with correct values", {
  input_df <- tibble::tibble(
    option_name = c("A", "B", "C"),
    sum_clicks = c(1000, 1000, 1000),
    sum_conversions = c(100, 120, 110)
  )
  output <- estimate_win_prob(input_df, "conversion_rate")
  all_option_names <- input_df$option_name
  expected_column_names <- c("option_name", "win_prob_raw", "win_prob")
  expect_true(is.data.frame(output))
  expect_true(all(expected_column_names %in% colnames(output)))
  expect_true(all(output$option_name %in% all_option_names))
  expect_true(is.double(output$win_prob_raw))
  expect_true(is.character(output$win_prob))
  expect_equal(nrow(output), length(all_option_names))
})

test_that("estimate_win_prob returns tibble with correct values when win prob is 0", {
  input_df <- tibble::tibble(
    option_name = c("A", "B", "C"),
    sum_clicks = c(1000, 1000, 1000),
    sum_conversions = c(1, 120, 2)
  )
  output <- estimate_win_prob(input_df, "conversion_rate")
  all_option_names <- input_df$option_name
  expected_column_names <- c("option_name", "win_prob_raw", "win_prob")
  expect_true(is.data.frame(output))
  expect_true(all(expected_column_names %in% colnames(output)))
  expect_true(all(output$option_name %in% all_option_names))
  expect_true(is.double(output$win_prob_raw))
  expect_true(is.character(output$win_prob))
  expect_equal(nrow(output), length(all_option_names))
})
