context("Sample CTR")

test_that("sample_ctr returns correct shape", {
  input_df <- tibble::tibble(
    option_name = c("A", "B", "C"),
    sum_impressions = c(10000, 10000, 10000),
    sum_clicks = c(1000, 950, 1050)
  )
  n_options <- length(unique(input_df$option_name))
  n_samples <- 150
  expected_col_names <- c("option_name", "sum_impressions", "sum_clicks",
                          "sum_conversions", "beta_params", "samples")
  output <- sample_ctr(input_df, priors = list(), n_samples = n_samples)
  expect_true(is.data.frame(output))
  expect_true(all(c("option_name", "samples") %in% colnames(output)))
  expect_length(output$samples, n_options)
  purrr::walk(output$samples, ~ expect_length(.x, n_samples))
  expect_equal(colnames(output), expected_col_names)
})
