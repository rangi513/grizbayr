context("Estimate Win Probability vs Baseline Given Posterior")

test_that("estimate_win_prob_vs_baseline_given_posterior returns only 2 options", {
  input_df <- tibble::tibble(
    option_name = c("A", "B", "C"),
    sum_clicks = c(1000, 1000, 1000),
    sum_conversions = c(100, 120, 110)
  )
  posterior_samples <- sample_from_posterior(input_df, "conversion_rate", priors = list())
  all_option_names <- unique(posterior_samples$option_name)
  output <- estimate_win_prob_vs_baseline_given_posterior(posterior_samples = posterior_samples,
                                                          "conversion_rate",
                                                          "A")
  # Subset exists
  expect_true(all(output$option_name %in% all_option_names))
  expect_true(nrow(output) == 2)
})


test_that("estimate_win_prob_vs_baseline_given_posterior handles when wrt_option is the best", {
  # Can't be better than yourself
  input_df <- tibble::tibble(
    option_name = c("A", "B", "C"),
    sum_clicks = c(1000, 1000, 1000),
    sum_conversions = c(100, 120, 110)
  )
  posterior_samples <- sample_from_posterior(input_df, "conversion_rate", priors = list())
  all_option_names <- unique(posterior_samples$option_name)
  output <- estimate_win_prob_vs_baseline_given_posterior(posterior_samples = posterior_samples,
                                                          distribution = "conversion_rate",
                                                          wrt_option = "B")
  expect_true(all(output$option_name %in% all_option_names))
  expect_true(nrow(output) == 1)
})
