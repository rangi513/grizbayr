context("Sample Session Duration")

test_that("sample_session_duration returns correct shape", {
  input_df <- tibble::tibble(
    option_name = c("A", "B", "C"),
    sum_sessions = c(1000, 500, 1000),
    sum_duration = c(50000, 60000, 35000),
  )
  n_options <- length(unique(input_df$option_name))
  n_samples <- 150
  expected_col_names <- c(colnames(input_df), "gamma_params", "samples")
  output <- sample_session_duration(input_df, priors = list(), n_samples = n_samples)
  expect_true(is.data.frame(output))
  expect_true(all(c("option_name", "samples") %in% colnames(output)))
  expect_length(output$samples, n_options)
  purrr::walk(output$samples, ~ expect_length(.x, n_samples))
  expect_equal(colnames(output), expected_col_names)
})
