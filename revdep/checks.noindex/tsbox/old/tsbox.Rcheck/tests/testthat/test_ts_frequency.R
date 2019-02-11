library(testthat)
library(tsbox)

context("ts_frequency")
test_that("ts_frequency survives freq conversion", {
  expect_equal(
    ts_frequency(EuStockMarkets, 1),
    ts_ts(ts_frequency(ts_xts(EuStockMarkets), 1))
  )
})
