context("Sample From Posterior")

test_that("sample_from_posterior returns correct dataframe shape: conversion_rate", {
  input_df <- tibble::tibble(
     option_name = c("A", "B", "C"),
     sum_clicks = c(1000, 1000, 1000),
     sum_conversions = c(100, 120, 110)
  )
  n_samples <- 150
  n_unique_options <- length(unique(input_df$option_name))
  expected_col_names <- c("option_name", "samples", "sample_id")
  output <- sample_from_posterior(input_df, "conversion_rate", n_samples = n_samples)
  expect_true(is.data.frame(output))
  expect_length(output, n_unique_options)
  expect_equal(nrow(output), n_unique_options * n_samples)
  expect_true(all(colnames(output) == expected_col_names))
})

test_that("sample_from_posterior returns correct dataframe shape for all types", {
  input_df_all <- tibble::tibble(
    option_name = c("A", "B", "C"),
    sum_impressions = c(10000, 9000, 11000),
    sum_sessions = c(1000, 1000, 1000),
    sum_conversions = c(100, 120, 110),
    sum_revenue = c(900, 1200, 1150),
    sum_cost = c(10, 50, 30),
    sum_conversions_2 = c(10, 8, 20),
    sum_revenue_2 = c(10, 16, 15),
    sum_duration = c(5000, 3000, 4000),
    sum_page_views = c(3000, 2000, 1345)
  ) %>%
    dplyr::mutate(sum_clicks = sum_sessions)
  n_samples <- 150
  n_unique_options <- length(unique(input_df_all$option_name))
  expected_col_names <- c("option_name", "samples", "sample_id")

  output <- purrr::map(distribution_column_mapping$distribution_type,
                       ~ sample_from_posterior(input_df_all,
                                               .x,
                                               n_samples = n_samples))

  expect_true(purrr::every(output, ~ is.data.frame(.x)))
  purrr::walk(output, ~ expect_length(.x, n_unique_options))
  purrr::walk(output, ~ expect_equal(nrow(.x), n_unique_options * n_samples))
  expect_true(purrr::every(output, ~ all(colnames(.x) == expected_col_names)))
})

test_that("sample_from_posterior fails when incorrect distribution is input", {
  input_df <- tibble::tibble(
    option_name = c("A", "B", "C"),
    sum_clicks = c(1000, 1000, 1000),
    sum_conversions = c(100, 120, 110)
  )
  expect_error(sample_from_posterior(input_df, "bad_input", n_samples = n_samples))
})
