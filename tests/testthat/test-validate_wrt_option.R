context("Validate With Respect To Option")

test_that("validate_wrt_option fails when not in option name", {
  post_samples_example <- tibble::tibble(option_name = c("X", "Y", "Z"))
  expect_error({
    validate_wrt_option(wrt_option = "B", posterior_samples = post_samples_example)
  })
})


test_that("validate_wrt_option passes when in option name", {
  post_samples_example <- tibble::tibble(option_name = c("A", "B", "C"))
  expect_invisible({
    validate_wrt_option(wrt_option = "B", posterior_samples = post_samples_example)
  })
})
