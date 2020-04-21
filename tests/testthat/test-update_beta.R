context("Update Beta")

test_that("update_beta updates with priors", {
  expected_output <- tibble::tibble(alpha = 3, beta = 7)
  output <- update_beta(alpha = 1, beta = 5, priors = list(alpha0 = 2, beta0 = 2))
  testthat::expect_equal(output, expected_output)
})

test_that("update_beta updates without priors", {
  expected_output <- tibble::tibble(alpha = 2, beta = 6)
  output <- update_beta(alpha = 1, beta = 5)
  testthat::expect_equal(output, expected_output)
})

test_that("update_beta updates but warns with invalid priors", {
  expected_output <- tibble::tibble(alpha = 2, beta = 6)
  invalid_priors <- list(alpha0 = -1, beta0 = 3)
  testthat::expect_warning({
    output <- update_beta(alpha = 1, beta = 5, priors = invalid_priors)
  })
  testthat::expect_equal(output, expected_output)
})

test_that("update_beta updates without priors large numbers", {
  expected_output <- tibble::tibble(alpha = 20001, beta = 50001)
  output <- update_beta(alpha = 20000, beta = 50000)
  testthat::expect_equal(output, expected_output)
})
