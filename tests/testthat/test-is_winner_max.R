context("Is Winner Max")

test_that("is_winner_max returns FALSE when CPC or CPA is input", {
  expect_false(is_winner_max("cpc"))
  expect_false(is_winner_max("cpa"))
})

test_that("is_winner_max returns TRUE when anything else is input", {
  expect_true(is_winner_max("conversion_rate"))
  expect_true(is_winner_max("something_random"))
})
