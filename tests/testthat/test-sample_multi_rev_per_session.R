context("Sample Multi Rev Per Session")

test_that("sample_multi_rev_per_session returns correct shape", {
  input_df <- tibble::tibble(
    option_name = c("A", "B", "C"),
    sum_sessions = c(1000, 1000, 1000),
    sum_conversions = c(100, 120, 110),
    sum_revenue = c(900, 1200, 1150),
    sum_conversions_2 = c(10, 8, 20),
    sum_revenue_2 = c(10, 16, 15)
  )
  n_options <- length(unique(input_df$option_name))
  n_samples <- 150
  expected_col_names <- c(colnames(input_df), "dirichlet_params",
                           "gamma_params_A", "gamma_params_B", "samples")
  output <- sample_multi_rev_per_session(input_df, priors = list(), n_samples = n_samples)
  expect_true(is.data.frame(output))
  expect_true(all(c("option_name", "samples") %in% colnames(output)))
  expect_length(output$samples, n_options)
  purrr::walk(output$samples, ~ expect_length(.x, n_samples))
  expect_equal(sort(colnames(output)), sort(expected_col_names))
})
