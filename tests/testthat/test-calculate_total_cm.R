context("Calculate Total CM")

test_that("calculate_total_cm returns correct cm", {
  expected_ouput <- 15
  output <- calculate_total_cm(rev_per_click = 10, cost_per_click = 5, expected_clicks = 3)
  expect_equal(output, expected_ouput)
})

test_that("calculate_total_cm returns negative value", {
  expected_ouput <- -30
  output <- calculate_total_cm(rev_per_click = 10, cost_per_click = 20, expected_clicks = 3)
  expect_equal(output, expected_ouput)
})

test_that("calculate_total_cm returns 0 when there are no clicks", {
  expected_ouput <- 0
  output <- calculate_total_cm(rev_per_click = 10, cost_per_click = 5, expected_clicks = 0)
  expect_equal(output, expected_ouput)
})
