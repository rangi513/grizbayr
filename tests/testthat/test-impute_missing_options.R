context("Impute Missing Options")

test_that("impute_missing_options adds a row if it doesn't exist", {
  expected_output <- tibble::tibble(option_name = c("B", "C", "A"), win_prob_raw = c(0.4, 0.6, 0.0))
  post_sample_example <- tibble::tibble(option_name = c("A", "B", "C"))
  wp_raw <- tibble::tibble(option_name = c("B", "C"),
                           win_prob_raw = c(0.4, 0.6))
  output <- impute_missing_options(posterior_samples = post_sample_example,
                                   wp_raw = wp_raw)
  expect_true(is.data.frame(output))
  expect_equal(output, expected_output)
})

test_that("impute_missing_options adds multiple rows if they don't exist", {
  expected_output <- tibble::tibble(option_name = c("C", "A", "B"), win_prob_raw = c(1.0, 0.0, 0.0))
  post_sample_example <- tibble::tibble(option_name = c("A", "B", "C"))
  wp_raw <- tibble::tibble(option_name = c("C"),
                           win_prob_raw = c(1.0))
  output <- impute_missing_options(posterior_samples = post_sample_example,
                                   wp_raw = wp_raw)
  expect_true(is.data.frame(output))
  expect_equal(output, expected_output)
})

test_that("impute_missing_options doesn't add anything if all exist", {
  expected_output <- tibble::tibble(option_name = c("A", "B", "C"), win_prob_raw = c(0.3, 0.4, 0.3))
  post_sample_example <- tibble::tibble(option_name = c("A", "B", "C"))
  output <- impute_missing_options(posterior_samples = post_sample_example,
                                   wp_raw = expected_output)
  expect_true(is.data.frame(output))
  expect_equal(output, expected_output)
})

