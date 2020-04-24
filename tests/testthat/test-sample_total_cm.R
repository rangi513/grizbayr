context("Sample Total CM")

test_that("sample_total_cm returns correct shape", {
  input_df_cm <- tibble::tibble(
    option_name = c("A", "B", "C"),
    sum_impressions = c(10000, 10000, 10000),
    sum_clicks = c(1000, 1000, 1000),
    sum_conversions = c(100, 120, 110),
    sum_revenue = c(900, 1200, 1150),
    sum_cost = c(10, 50, 30),
  )
  n_options <- length(unique(input_df_cm$option_name))
  n_samples <- 150
  expected_col_names <- c(colnames(input_df_cm), "beta_params_conv", "beta_params_ctr",
                          "gamma_params_rev", "gamma_params_cost", "samples")
  output <- sample_total_cm(input_df_cm, priors = list(), n_samples = n_samples)
  expect_true(is.data.frame(output))
  expect_true(all(c("option_name", "samples") %in% colnames(output)))
  expect_length(output$samples, n_options)
  purrr::walk(output$samples, ~ expect_length(.x, n_samples))
  expect_equal(colnames(output), expected_col_names)
})
