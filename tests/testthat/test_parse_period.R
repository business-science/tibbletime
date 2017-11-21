context("parse_period testing")

# Tests

test_that("Basic parsing", {
  expect_equal(parse_period("d"), list(freq = 1, period = "day"))
  expect_equal(parse_period(2~d), list(freq = 2, period = "day"))
  expect_equal(parse_period(3~m), list(freq = 3, period = "month"))
})

test_that("Minute vs Month parsing works", {
  expect_equal(parse_period("M"), list(freq = 1, period = "min"))
  expect_equal(parse_period("m"), list(freq = 1, period = "month"))
})

test_that("Errors are thrown with incorrect specification", {
  expect_error(parse_period("t"), "Period specified incorrectly.")
  expect_error(parse_period(2~test), "Period specified incorrectly.")
  expect_error(parse_period(test~y), "LHS of `period` formula must be numeric.")
})
