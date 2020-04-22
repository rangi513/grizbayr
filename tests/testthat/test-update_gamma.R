context("Update Gamma")

test_that("update_gamma updates with priors", {
  expected_output <- tibble::tibble(k = 3, theta = 1000/(1 + 1000 * 100))
  output <- update_gamma(k = 1, theta = 100, priors = list(k0 = 2, theta0 = 1000))
  testthat::expect_equal(output, expected_output)
})

test_that("update_gamma updates without priors", {
  expected_output <- tibble::tibble(k = 11, theta = 250/(1 + 250 * 200))
  output <- update_gamma(k = 10, theta = 200)
  testthat::expect_equal(output, expected_output)
})

test_that("update_gamma updates but warns with invalid priors", {
  expected_output <- tibble::tibble(k = 11, theta = 250/(1 + 250 * 200))
  invalid_priors <- list(k0 = -2, theta0 = 1000)
  testthat::expect_warning({
    output <- update_gamma(k = 10, theta = 200, priors = invalid_priors)
  })
  testthat::expect_equal(output, expected_output)
})

test_that("update_gamma updates with alternate priors", {
  expected_output <- tibble::tibble(k = 3, theta = 1000/(1 + 1000 * 100))
  output <- update_gamma(k = 1, theta = 100, priors = list(k01 = 2, theta01 = 1000), alternate_priors = TRUE)
  testthat::expect_equal(output, expected_output)
})

test_that("update_gamma uses default priors with only alternate priors when set to FALSE", {
  expected_output <- tibble::tibble(k = 2, theta = 250/(1 + 250 * 100))
  testthat::expect_warning(output <- update_gamma(k = 1,
                                                  theta = 100,
                                                  priors = list(k01 = 2, theta01 = 1000),
                                                  alternate_priors = FALSE)
                           )
  testthat::expect_equal(output, expected_output)
})
