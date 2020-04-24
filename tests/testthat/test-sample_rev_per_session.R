context("Sample Rev Per Session")

test_that("sample_rev_per_session returns correct shape", {
  input_df_cm_per_click <- tibble::tibble(
    option_name = c("A", "B", "C"),
    sum_sessions = c(1000, 1000, 1000),
    sum_conversions = c(100, 120, 110),
    sum_revenue = c(900, 1200, 1150)
  )
  n_options <- length(unique(input_df_cm_per_click$option_name))
  n_samples <- 150
  expected_col_names <- c(colnames(input_df_cm_per_click),
                          "beta_params", "gamma_params", "samples")
  output <- sample_rev_per_session(input_df_cm_per_click, priors = list(), n_samples = n_samples)
  expect_true(is.data.frame(output))
  expect_true(all(c("option_name", "samples") %in% colnames(output)))
  expect_length(output$samples, n_options)
  purrr::walk(output$samples, ~ expect_length(.x, n_samples))
  expect_equal(colnames(output), expected_col_names)
})
