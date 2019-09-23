library(testthat)
library(tsbox)

context("ts_frequency")
test_that("ts_frequency survives freq conversion", {
  expect_equal(
    ts_frequency(EuStockMarkets, 1),
    ts_ts(ts_frequency(ts_xts(EuStockMarkets), 1))
  )
})

test_that("ts_frequency handles na.rm correctly", {

  x <- ts_c(mdeaths, austres)
  window(x, start = c(1985, 6), end = c(1985, 12)) <- NA

  x0 <- ts_frequency(x)
  x1 <- ts_frequency(x, na.rm = TRUE)

  expect_identical(colnames(x0), colnames(x))
  expect_identical(colnames(x1), colnames(x))

  expect_true(is.na(window(x0, start = 1985, end = 1985)[, 'austres']))
  expect_false(is.na(window(x1, start = 1985, end = 1985)[, 'austres']))
})
