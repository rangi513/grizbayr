context("Calculate Mult Rev Per Session")

test_that("calculate_multi_rev_per_session returns correct calculations", {
  expected_ouput <- 45
  output <- calculate_multi_rev_per_session(conv_rates = list(alpha_1 = 0.5, alpha_2 = 0.2),
                                            inverse_rev_A = 0.1,
                                            inverse_rev_B = 0.005)
  expect_equal(output, expected_ouput)
})

test_that("calculate_multi_rev_per_session returns 0 when rates are 0", {
  expected_ouput <- 0
  output <- calculate_multi_rev_per_session(conv_rates = list(alpha_1 = 0, alpha_2 = 0),
                                            inverse_rev_A = 0.1,
                                            inverse_rev_B = 0.005)
  expect_equal(output, expected_ouput)
})
