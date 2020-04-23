context("Estimate Win Probability Given Posterior")

test_that("estimate_win_prob_given_posterior returns correct results", {
  input_df <- tibble::tibble(
    option_name = c("A", "B", "C"),
    sum_clicks = c(1000, 1000, 1000),
    sum_conversions = c(100, 120, 110)
  )
  posterior_samples <- sample_from_posterior(input_df, "conversion_rate", priors = list())
  all_option_names <- unique(posterior_samples$option_name)

  output <- estimate_win_prob_given_posterior(posterior_samples = posterior_samples, winner_is_max = TRUE)
  expected_column_names <- c("option_name", "win_prob_raw", "win_prob")

  expect_true(is.data.frame(output))
  expect_true(all(expected_column_names %in% colnames(output)))
  expect_true(all(output$option_name %in% all_option_names))
  expect_true(is.double(output$win_prob_raw))
  expect_true(is.character(output$win_prob))
  expect_equal(nrow(output), length(all_option_names))
})

test_that("estimate_win_prob_given_posterior returns correct results when winner_is_max = FALSE", {
  input_df <- tibble::tibble(
    option_name = c("A", "B", "C"),
    sum_clicks = c(1000, 1000, 1000),
    sum_conversions = c(100, 120, 110)
  )
  posterior_samples <- sample_from_posterior(input_df, "conversion_rate", priors = list())
  all_option_names <- unique(posterior_samples$option_name)

  output <- estimate_win_prob_given_posterior(posterior_samples = posterior_samples, winner_is_max = FALSE)
  expected_column_names <- c("option_name", "win_prob_raw", "win_prob")
  win_prob_of_lowest <- as.double(output[output$option_name == "A","win_prob_raw"][1])

  expect_equal(win_prob_of_lowest, max(output$win_prob_raw))

  expect_true(is.data.frame(output))
  expect_true(all(expected_column_names %in% colnames(output)))
  expect_true(all(output$option_name %in% all_option_names))
  expect_true(is.double(output$win_prob_raw))
  expect_true(is.character(output$win_prob))
  expect_equal(nrow(output), length(all_option_names))
})
