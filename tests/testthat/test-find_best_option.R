context("Find Best Option")

test_that("find_best_option returns the correct best option when max is best", {
  expected_output <- "B"
  input_df <- tibble::tibble(
    option_name = c("A", "B", "C"),
    sum_clicks = c(1000, 1000, 1000),
    sum_conversions = c(100, 120, 110)
  )
  posterior_samples <- sample_from_posterior(input_df, "conversion_rate")
  output <- find_best_option(posterior_samples = posterior_samples,
                             distribution = "conversion_rate")
  expect_equal(output, expected_output)
})

test_that("find_best_option returns the correct best option when min is best", {
  expected_output <- "C"
  input_df <- tibble::tibble(
    option_name = c("A", "B", "C"),
    sum_clicks = c(1000, 1000, 1000),
    sum_conversions = c(100, 120, 110),
    sum_cost = c(150, 200, 100),
  )
  posterior_samples <- sample_from_posterior(input_df, "cpa")
  output <- find_best_option(posterior_samples = posterior_samples,
                             distribution = "cpa")
  expect_equal(output, expected_output)
})


test_that("find_best_option returns when 2 options are equal", {
  expected_output <- c("B","C")
  input_df <- tibble::tibble(
    option_name = c("A", "B", "C"),
    sum_clicks = c(1000, 1000, 1000),
    sum_conversions = c(100, 120, 120),
  )
  posterior_samples <- sample_from_posterior(input_df, "conversion_rate")
  output <- find_best_option(posterior_samples = posterior_samples,
                             distribution = "conversion_rate")
  expect_true(all(output %in% expected_output))
})
