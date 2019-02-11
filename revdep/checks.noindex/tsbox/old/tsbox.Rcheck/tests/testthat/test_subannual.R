library(testthat)
library(tsbox)

context("irregular conversion handling")

test_that("conversion produces right classes for monthly series", {
  monthly_series <- window(AirPassengers, start = c(1949, 5))
  expect_s3_class(ts_xts(monthly_series), "xts")
  expect_s3_class(ts_ts(monthly_series), "ts")
  expect_s3_class(ts_df(monthly_series), "data.frame")
  expect_s3_class(ts_dt(monthly_series), "data.table")
  expect_s3_class(ts_tbl(monthly_series), "tbl_df")

  expect_s3_class(ts_xts(ts_xts(monthly_series)), "xts")
  expect_s3_class(ts_ts(ts_xts(monthly_series)), "ts")
  expect_s3_class(ts_df(ts_xts(monthly_series)), "data.frame")
  expect_s3_class(ts_dt(ts_xts(monthly_series)), "data.table")
  expect_s3_class(ts_tbl(ts_xts(monthly_series)), "tbl_df")

  expect_s3_class(ts_xts(ts_df(monthly_series)), "xts")
  expect_s3_class(ts_ts(ts_df(monthly_series)), "ts")
  expect_s3_class(ts_df(ts_df(monthly_series)), "data.frame")
  expect_s3_class(ts_dt(ts_df(monthly_series)), "data.table")
  expect_s3_class(ts_tbl(ts_df(monthly_series)), "tbl_df")

  expect_s3_class(ts_xts(ts_dt(monthly_series)), "xts")
  expect_s3_class(ts_ts(ts_dt(monthly_series)), "ts")
  expect_s3_class(ts_df(ts_dt(monthly_series)), "data.frame")
  expect_s3_class(ts_dt(ts_dt(monthly_series)), "data.table")
  expect_s3_class(ts_tbl(ts_dt(monthly_series)), "tbl_df")

  expect_s3_class(ts_xts(ts_tbl(monthly_series)), "xts")
  expect_s3_class(ts_ts(ts_tbl(monthly_series)), "ts")
  expect_s3_class(ts_df(ts_tbl(monthly_series)), "data.frame")
  expect_s3_class(ts_dt(ts_tbl(monthly_series)), "data.table")
  expect_s3_class(ts_tbl(ts_tbl(monthly_series)), "tbl_df")
})


test_that("conversion between objects works as expected for monthly series", {
  x.ts <- window(AirPassengers, start = c(1949, 5))
  x.xts <- ts_xts(x.ts)
  x.df <- ts_df(x.xts)
  x.dt <- ts_dt(x.df)
  x.tbl <- ts_tbl(x.dt)

  expect_equal(ts_ts(ts_xts(x.ts)), x.ts)
  expect_equal(ts_ts(ts_df(x.ts)), x.ts)
  expect_equal(ts_ts(ts_dt(x.ts)), x.ts)
  expect_equal(ts_ts(ts_tbl(x.ts)), x.ts)

  expect_equal(ts_xts(ts_ts(x.xts)), x.xts)
  expect_equal(ts_xts(ts_df(x.xts)), x.xts)
  expect_equal(ts_xts(ts_dt(x.xts)), x.xts)
  expect_equal(ts_xts(ts_tbl(x.xts)), x.xts)

  expect_equal(ts_df(ts_ts(x.df)), x.df)
  expect_equal(ts_df(ts_xts(x.df)), x.df)
  expect_equal(ts_df(ts_dt(x.df)), x.df)
  expect_equal(ts_df(ts_tbl(x.df)), x.df)

  expect_equal(ts_dt(ts_ts(x.dt)), x.dt)
  expect_equal(ts_dt(ts_xts(x.dt)), x.dt)
  expect_equal(ts_dt(ts_df(x.dt)), x.dt)
  expect_equal(ts_dt(ts_tbl(x.dt)), x.dt)

  expect_equal(ts_tbl(ts_ts(x.tbl)), x.tbl)
  expect_equal(ts_tbl(ts_xts(x.tbl)), x.tbl)
  expect_equal(ts_tbl(ts_df(x.tbl)), x.tbl)
  expect_equal(ts_tbl(ts_dt(x.tbl)), x.tbl)
})


test_that("conversion produces right classes for regular series", {
  regular_series <- window(EuStockMarkets, start = c(1991, 200))
  expect_s3_class(ts_xts(regular_series), "xts")
  expect_s3_class(ts_ts(regular_series), "ts")
  expect_s3_class(ts_df(regular_series), "data.frame")
  expect_s3_class(ts_dt(regular_series), "data.table")
  expect_s3_class(ts_tbl(regular_series), "tbl_df")

  expect_s3_class(ts_xts(ts_xts(regular_series)), "xts")
  expect_s3_class(ts_ts(ts_xts(regular_series)), "ts")
  expect_s3_class(ts_df(ts_xts(regular_series)), "data.frame")
  expect_s3_class(ts_dt(ts_xts(regular_series)), "data.table")
  expect_s3_class(ts_tbl(ts_xts(regular_series)), "tbl_df")

  expect_s3_class(ts_xts(ts_df(regular_series)), "xts")
  expect_s3_class(ts_ts(ts_df(regular_series)), "ts")
  expect_s3_class(ts_df(ts_df(regular_series)), "data.frame")
  expect_s3_class(ts_dt(ts_df(regular_series)), "data.table")
  expect_s3_class(ts_tbl(ts_df(regular_series)), "tbl_df")

  expect_s3_class(ts_xts(ts_dt(regular_series)), "xts")
  expect_s3_class(ts_ts(ts_dt(regular_series)), "ts")
  expect_s3_class(ts_df(ts_dt(regular_series)), "data.frame")
  expect_s3_class(ts_dt(ts_dt(regular_series)), "data.table")
  expect_s3_class(ts_tbl(ts_dt(regular_series)), "tbl_df")

  expect_s3_class(ts_xts(ts_tbl(regular_series)), "xts")
  expect_s3_class(ts_ts(ts_tbl(regular_series)), "ts")
  expect_s3_class(ts_df(ts_tbl(regular_series)), "data.frame")
  expect_s3_class(ts_dt(ts_tbl(regular_series)), "data.table")
  expect_s3_class(ts_tbl(ts_tbl(regular_series)), "tbl_df")
})


test_that("conversion between objects works as expected for regular series", {
  x.ts <- window(EuStockMarkets, start = c(1991, 200))
  x.xts <- ts_xts(x.ts)
  x.df <- ts_df(x.xts)
  x.dt <- ts_dt(x.df)
  x.tbl <- ts_tbl(x.dt)

  expect_equal(ts_ts(ts_xts(x.ts)), x.ts)
  expect_equal(ts_ts(ts_df(x.ts)), x.ts)
  expect_equal(ts_ts(ts_dt(x.ts)), x.ts)
  expect_equal(ts_ts(ts_tbl(x.ts)), x.ts)

  expect_equal(ts_xts(ts_ts(x.xts)), x.xts)
  expect_equal(ts_xts(ts_df(x.xts)), x.xts)
  expect_equal(ts_xts(ts_dt(x.xts)), x.xts)
  expect_equal(ts_xts(ts_tbl(x.xts)), x.xts)

  expect_equal(ts_df(ts_ts(x.df)), x.df)
  expect_equal(ts_df(ts_xts(x.df)), x.df)
  expect_equal(ts_df(ts_dt(x.df)), x.df)
  expect_equal(ts_df(ts_tbl(x.df)), x.df)

  expect_equal(ts_dt(ts_ts(x.dt)), x.dt)
  expect_equal(ts_dt(ts_xts(x.dt)), x.dt)
  expect_equal(ts_dt(ts_df(x.dt)), x.dt)
  expect_equal(ts_dt(ts_tbl(x.dt)), x.dt)

  expect_equal(ts_tbl(ts_ts(x.tbl)), x.tbl)
  expect_equal(ts_tbl(ts_xts(x.tbl)), x.tbl)
  expect_equal(ts_tbl(ts_df(x.tbl)), x.tbl)
  expect_equal(ts_tbl(ts_dt(x.tbl)), x.tbl)
})


test_that("conversion produces right classes for quarterly series", {
  quarter_series <- window(austres, start = c(1975, 3))
  expect_s3_class(ts_xts(quarter_series), "xts")
  expect_s3_class(ts_ts(quarter_series), "ts")
  expect_s3_class(ts_df(quarter_series), "data.frame")
  expect_s3_class(ts_dt(quarter_series), "data.table")
  expect_s3_class(ts_tbl(quarter_series), "tbl_df")

  expect_s3_class(ts_xts(ts_xts(quarter_series)), "xts")
  expect_s3_class(ts_ts(ts_xts(quarter_series)), "ts")
  expect_s3_class(ts_df(ts_xts(quarter_series)), "data.frame")
  expect_s3_class(ts_dt(ts_xts(quarter_series)), "data.table")
  expect_s3_class(ts_tbl(ts_xts(quarter_series)), "tbl_df")

  expect_s3_class(ts_xts(ts_df(quarter_series)), "xts")
  expect_s3_class(ts_ts(ts_df(quarter_series)), "ts")
  expect_s3_class(ts_df(ts_df(quarter_series)), "data.frame")
  expect_s3_class(ts_dt(ts_df(quarter_series)), "data.table")
  expect_s3_class(ts_tbl(ts_df(quarter_series)), "tbl_df")

  expect_s3_class(ts_xts(ts_dt(quarter_series)), "xts")
  expect_s3_class(ts_ts(ts_dt(quarter_series)), "ts")
  expect_s3_class(ts_df(ts_dt(quarter_series)), "data.frame")
  expect_s3_class(ts_dt(ts_dt(quarter_series)), "data.table")
  expect_s3_class(ts_tbl(ts_dt(quarter_series)), "tbl_df")

  expect_s3_class(ts_xts(ts_tbl(quarter_series)), "xts")
  expect_s3_class(ts_ts(ts_tbl(quarter_series)), "ts")
  expect_s3_class(ts_df(ts_tbl(quarter_series)), "data.frame")
  expect_s3_class(ts_dt(ts_tbl(quarter_series)), "data.table")
  expect_s3_class(ts_tbl(ts_tbl(quarter_series)), "tbl_df")
})


test_that("conversion between objects works as expected: austres", {
  x.ts <- window(austres, start = c(1975, 3))
  x.xts <- ts_xts(x.ts)
  x.df <- ts_df(x.xts)
  x.dt <- ts_dt(x.df)
  x.tbl <- ts_tbl(x.dt)

  expect_equal(ts_ts(ts_xts(x.ts)), x.ts)
  expect_equal(ts_ts(ts_df(x.ts)), x.ts)
  expect_equal(ts_ts(ts_dt(x.ts)), x.ts)
  expect_equal(ts_ts(ts_tbl(x.ts)), x.ts)

  expect_equal(ts_xts(ts_ts(x.xts)), x.xts)
  expect_equal(ts_xts(ts_df(x.xts)), x.xts)
  expect_equal(ts_xts(ts_dt(x.xts)), x.xts)
  expect_equal(ts_xts(ts_tbl(x.xts)), x.xts)

  expect_equal(ts_df(ts_ts(x.df)), x.df)
  expect_equal(ts_df(ts_xts(x.df)), x.df)
  expect_equal(ts_df(ts_dt(x.df)), x.df)
  expect_equal(ts_df(ts_tbl(x.df)), x.df)

  expect_equal(ts_dt(ts_ts(x.dt)), x.dt)
  expect_equal(ts_dt(ts_xts(x.dt)), x.dt)
  expect_equal(ts_dt(ts_df(x.dt)), x.dt)
  expect_equal(ts_dt(ts_tbl(x.dt)), x.dt)

  expect_equal(ts_tbl(ts_ts(x.tbl)), x.tbl)
  expect_equal(ts_tbl(ts_xts(x.tbl)), x.tbl)
  expect_equal(ts_tbl(ts_df(x.tbl)), x.tbl)
  expect_equal(ts_tbl(ts_dt(x.tbl)), x.tbl)
})


test_that("conversion produces right classes for series with NAs", {
  irregular_series <- window(AirPassengers, start = c(1951, 3))
  window(irregular_series, start = c(1951, 4), end = c(1951, 4)) <- NA

  expect_s3_class(ts_xts(irregular_series), "xts")
  expect_s3_class(ts_ts(irregular_series), "ts")
  expect_s3_class(ts_df(irregular_series), "data.frame")
  expect_s3_class(ts_dt(irregular_series), "data.table")
  expect_s3_class(ts_tbl(irregular_series), "tbl_df")

  expect_s3_class(ts_xts(ts_xts(irregular_series)), "xts")
  expect_s3_class(ts_ts(ts_xts(irregular_series)), "ts")
  expect_s3_class(ts_df(ts_xts(irregular_series)), "data.frame")
  expect_s3_class(ts_dt(ts_xts(irregular_series)), "data.table")
  expect_s3_class(ts_tbl(ts_xts(irregular_series)), "tbl_df")

  expect_s3_class(ts_xts(ts_df(irregular_series)), "xts")
  expect_s3_class(ts_ts(ts_df(irregular_series)), "ts")
  expect_s3_class(ts_df(ts_df(irregular_series)), "data.frame")
  expect_s3_class(ts_dt(ts_df(irregular_series)), "data.table")
  expect_s3_class(ts_tbl(ts_df(irregular_series)), "tbl_df")

  expect_s3_class(ts_xts(ts_dt(irregular_series)), "xts")
  expect_s3_class(ts_ts(ts_dt(irregular_series)), "ts")
  expect_s3_class(ts_df(ts_dt(irregular_series)), "data.frame")
  expect_s3_class(ts_dt(ts_dt(irregular_series)), "data.table")
  expect_s3_class(ts_tbl(ts_dt(irregular_series)), "tbl_df")

  expect_s3_class(ts_xts(ts_tbl(irregular_series)), "xts")
  expect_s3_class(ts_ts(ts_tbl(irregular_series)), "ts")
  expect_s3_class(ts_df(ts_tbl(irregular_series)), "data.frame")
  expect_s3_class(ts_dt(ts_tbl(irregular_series)), "data.table")
  expect_s3_class(ts_tbl(ts_tbl(irregular_series)), "tbl_df")
})

test_that("conversion between objects works as expected for series with NAs", {
  x.ts <- window(AirPassengers, start = c(1951, 3))
  window(x.ts, start = c(1951, 4), end = c(1951, 4)) <- NA


  x.xts <- ts_xts(x.ts)
  x.df <- ts_df(x.xts)
  x.dt <- ts_dt(x.df)
  x.tbl <- ts_tbl(x.dt)

  expect_equal(ts_ts(ts_xts(x.ts)), x.ts)
  expect_equal(ts_ts(ts_df(x.ts)), x.ts)
  expect_equal(ts_ts(ts_dt(x.ts)), x.ts)
  expect_equal(ts_ts(ts_tbl(x.ts)), x.ts)

  expect_equal(ts_xts(ts_ts(x.xts)), x.xts)
  expect_equal(ts_xts(ts_df(x.xts)), x.xts)
  expect_equal(ts_xts(ts_dt(x.xts)), x.xts)
  expect_equal(ts_xts(ts_tbl(x.xts)), x.xts)

  expect_equal(ts_df(ts_ts(x.df)), x.df)
  expect_equal(ts_df(ts_xts(x.df)), x.df)
  expect_equal(ts_df(ts_dt(x.df)), x.df)
  expect_equal(ts_df(ts_tbl(x.df)), x.df)

  expect_equal(ts_dt(ts_ts(x.dt)), x.dt)
  expect_equal(ts_dt(ts_xts(x.dt)), x.dt)
  expect_equal(ts_dt(ts_df(x.dt)), x.dt)
  expect_equal(ts_dt(ts_tbl(x.dt)), x.dt)

  expect_equal(ts_tbl(ts_ts(x.tbl)), x.tbl)
  expect_equal(ts_tbl(ts_xts(x.tbl)), x.tbl)
  expect_equal(ts_tbl(ts_df(x.tbl)), x.tbl)
  expect_equal(ts_tbl(ts_dt(x.tbl)), x.tbl)
})
