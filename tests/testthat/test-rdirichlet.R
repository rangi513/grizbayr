context("Random Dirichlet")

test_that("rdirichlet returns correct size df", {
  n_samples <- 100
  output <- rdirichlet(n_samples, list(a = 20, b = 15, c = 60))
  expect_true(is.data.frame(output))
  expect_true(nrow(output) == n_samples)
})

test_that("rdirichlet returns empty df when 0 samples are requested", {
  n_samples <- 0
  output <- rdirichlet(n_samples, list(a = 20, b = 15, c = 60))
  expect_true(is.data.frame(output))
  expect_true(nrow(output) == n_samples)
})

test_that("rdirichlet returns simplex for each row", {
  n_samples <- 100
  output <- rdirichlet(n_samples, list(a = 20, b = 15, c = 60))
  expect_true({
    purrr::pmap(output, function(...) round(sum(...), 2)) %>%
      purrr::every( ~ .x == 1)
  })
})

test_that("rdirichlet returns simplex for each row long list", {
  n_samples <- 100
  output <- rdirichlet(n_samples, list(a = 20, b = 15, c = 60, d = 30, f = 60, g = 22, h = 1, i = 0, k = 6))
  expect_true({
    purrr::pmap(output, function(...) round(sum(...), 2)) %>%
      purrr::every( ~ .x == 1)
  })
})

test_that("rdirichlet names match input", {
  n_samples <- 100
  input_list <- list(a = 20, b = 15, c = 60, d = 30, f = 60, g = 22, h = 1, i = 0, k = 6)
  output <- rdirichlet(n_samples, input_list)
  expect_true(all(names(output) == names(input_list)))
})
