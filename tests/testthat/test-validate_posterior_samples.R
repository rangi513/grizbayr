context("Validate Posterior Samples")

test_that("validate_posterior_samples fails if the input is not a dataframe", {
  input_list <- list(option_name = c("A", "B", "C"), samples = c(0,1,2))
  expect_error({
    validate_posterior_samples(posterior_samples = input_list)
  })
})
