library(testthat)
library(tsbox)

context("ts_pick")

test_that("ts_pick works", {
  # Programming use
  to.be.picked.and.renamed <- c(`My Dax` = "DAX", `My Smi` = "SMI")
  a <- ts_pick(EuStockMarkets, to.be.picked.and.renamed)
  b <- ts_pick(EuStockMarkets, `My Dax` = 'DAX', `My Smi` = 'SMI')
  expect_equal(a, b)

  b <- ts_pick(EuStockMarkets, `My Dax` = 1, `My Smi` = 2)
  expect_equal(a, b)

  expect_equal(EuStockMarkets[, c(1, 2)], ts_pick(EuStockMarkets, c(1, 2)))

})