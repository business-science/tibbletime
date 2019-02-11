library(testthat)
library(tsbox)


context("non heuristic series")

test_that("regular non standard series work with NA", {
  x0 <- EuStockMarkets
  x0[5:10, ] <- NA
  expect_equal(ts_ts(ts_tbl(x0)), x0)

  x0[c(100, 200), c(2, 4)] <- NA
  expect_equal(ts_ts(ts_tbl(x0)), x0)

})


