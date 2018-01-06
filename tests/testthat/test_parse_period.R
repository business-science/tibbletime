context("parse_period testing")

# Tests

test_that("Basic parsing", {
  expect_equal(parse_period('d'), list(freq = 1, period = "day"))
  expect_equal(parse_period('2 day'), list(freq = 2, period = "day"))
  expect_equal(parse_period('3 m'), list(freq = 3, period = "month"))
})

test_that("Minute vs Month parsing works", {
  expect_equal(parse_period("M"), list(freq = 1, period = "min"))
  expect_equal(parse_period("m"), list(freq = 1, period = "month"))
})

test_that("Errors are thrown with incorrect specification", {
  expect_error(parse_period("t"), "Period 't' specified incorrectly.")
  expect_error(parse_period('hi q'), "Frequency must be coercible to numeric.")
  expect_error(parse_period('2 test'), "Period 'test' specified incorrectly.")
})

test_that("Specialized subsecond parsing", {
  expect_lte(abs(parse_period('.1 sec')$freq - .1), 1e-9)
  expect_equal(parse_period('.1 sec')$period, "sec")

  expect_lte(abs(parse_period('1 millisec')$freq - .001), 1e-9)
  expect_equal(parse_period('1 millisec')$period, "sec")

  expect_lte(abs(parse_period('1 microsec')$freq - .000001), 1e-9)
  expect_equal(parse_period('1 microsec')$period, "sec")
})
