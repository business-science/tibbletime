library(testthat)
library(tsbox)

context("ts_lag")


test_that("integer and character shifting works the same", {
  expect_equal(ts_lag(mdeaths), ts_lag(mdeaths, "month"))
  expect_equal(ts_lag(austres), ts_lag(austres, "quarter"))

  expect_equal(ts_lag(mdeaths, 5), ts_lag(mdeaths, "5 month"))
  expect_equal(ts_lag(austres, -3), ts_lag(austres, "-3 quarter"))
  
  expect_equal(ts_lag(discoveries, -300), ts_lag(discoveries, "-300 years"))
  expect_equal(ts_lag(fdeaths, 11), ts_lag(fdeaths, "11 month"))

})



test_that("ts_lag works as stats::lag", {
  expect_equal(ts_lag(mdeaths), stats::lag(mdeaths, -1))
  expect_equal(ts_lag(mdeaths, -1), stats::lag(mdeaths, 1))

  expect_equal(ts_lag(mdeaths, 12), stats::lag(mdeaths, -12))
  expect_equal(ts_lag(mdeaths, -12), stats::lag(mdeaths, 12))

})

test_that("ts_lag works both ways", {
  expect_equal(ts_lag(ts_lag(mdeaths, -1)), ts_lag(ts_lag(mdeaths), -1))
  expect_equal(ts_lag(ts_lag(mdeaths, -12)), ts_lag(ts_lag(mdeaths), -12))

})

