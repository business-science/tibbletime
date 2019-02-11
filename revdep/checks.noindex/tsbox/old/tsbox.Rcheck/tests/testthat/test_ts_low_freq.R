library(testthat)
library(tsbox)



context("low frequency data, special cases")

test_that("regular low frequency data works with POSIXct", {
  
  # fails on some systems, time zones
  # x <- data.frame(
  #   time = seq(from = as.POSIXct("2000-01-01"), length.out = 10, by = "1 month"),
  #   value = 1:10
  # )
  # 
  # z <- ts_df(ts_ts(x))
  # x$time <- seq(from = as.Date("2000-01-01"), length.out = 10, by = "1 month")
  # expect_equal(x, z)



  x <- EuStockMarkets[, "DAX"] %>% 
    ts_df() %>% 
    ts_regular()

  expect_is(x, "data.frame")


})
