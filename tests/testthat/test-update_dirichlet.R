context("Update Dirichlet")

test_that("update_dirichlet adds 1 when default priors are used", {
  expected_output <- tibble::tibble(alpha_0 = 21, alpha_1 = 6, alpha_2 = 3)
  output <- update_dirichlet(alpha_0 = 20, alpha_1 = 5, alpha_2 = 2)
  expect_true(is.data.frame(output))
  expect_equal(output, expected_output)
})

test_that("update_dirichlet adds non default priors", {
  sample_priors_list <- list(alpha00 = 2, alpha01 = 3, alpha02 = 5)
  expected_output <- tibble::tibble(alpha_0 = 22, alpha_1 = 8, alpha_2 = 7)
  output <- update_dirichlet(alpha_0 = 20, alpha_1 = 5, alpha_2 = 2, priors = sample_priors_list)
  expect_true(is.data.frame(output))
  expect_equal(output, expected_output)
})

test_that("update_dirichlet adds 1 when default incorrect priors are used", {
  incorrect_priors_list <- list(beta00 = 2, beta01 = 3, beta02 = 5)
  expected_output <- tibble::tibble(alpha_0 = 21, alpha_1 = 6, alpha_2 = 3)
  expect_warning({
    output <- update_dirichlet(alpha_0 = 20, alpha_1 = 5, alpha_2 = 2, priors = incorrect_priors_list)
  })
  expect_true(is.data.frame(output))
  expect_equal(output, expected_output)
})
