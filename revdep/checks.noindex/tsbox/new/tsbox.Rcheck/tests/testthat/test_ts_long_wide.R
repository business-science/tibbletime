library(testthat)
library(tsbox)

context("long wide")

test_that("ts_long returns correct class.", {
  a <- ts_df(ts_c(ts_dt(AirPassengers), mdeaths, fdeaths))
  expect_equal(class(ts_long(ts_wide(a)))[1], "data.frame")
})


test_that("ts_long and ts_wide work both ways.", {
  a <- ts_df(ts_c(AirPassengers, mdeaths, fdeaths))
  expect_equal(a, ts_long(ts_wide(a)))

  b <- ts_tbl(ts_dt(EuStockMarkets))
  expect_equal(b, ts_long(ts_wide(b)))
})


test_that("ts_wide works has correct time stamps.", {
  a <- ts_df(ts_c(ts_dt(AirPassengers), mdeaths, fdeaths))
  expect_equal(ts_wide(ts_xts(a)), ts_xts(a))
  expect_equal(ts_wide(ts_ts(a)), ts_ts(a))
})

test_that("economics dataset can be converted to long format", {
  library(ggplot2)
  library(tsbox)
  expect_is(ts_long(economics), "tbl_df")
})

