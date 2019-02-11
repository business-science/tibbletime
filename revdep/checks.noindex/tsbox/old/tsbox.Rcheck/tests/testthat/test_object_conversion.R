library(testthat)
library(tsbox)


context("basic conversion handling")

test_that("conversion produces right classes", {

  skip_on_cran()

  expect_s3_class(ts_xts(AirPassengers), "xts")
  expect_s3_class(ts_ts(AirPassengers), "ts")
  expect_s3_class(ts_df(AirPassengers), "data.frame")
  expect_s3_class(ts_dt(AirPassengers), "data.table")
  expect_s3_class(ts_tbl(AirPassengers), "tbl_df")

  expect_s3_class(ts_xts(ts_xts(AirPassengers)), "xts")
  expect_s3_class(ts_ts(ts_xts(AirPassengers)), "ts")
  expect_s3_class(ts_df(ts_xts(AirPassengers)), "data.frame")
  expect_s3_class(ts_dt(ts_xts(AirPassengers)), "data.table")
  expect_s3_class(ts_tbl(ts_xts(AirPassengers)), "tbl_df")

  expect_s3_class(ts_xts(ts_df(AirPassengers)), "xts")
  expect_s3_class(ts_ts(ts_df(AirPassengers)), "ts")
  expect_s3_class(ts_df(ts_df(AirPassengers)), "data.frame")
  expect_s3_class(ts_dt(ts_df(AirPassengers)), "data.table")
  expect_s3_class(ts_tbl(ts_df(AirPassengers)), "tbl_df")

  expect_s3_class(ts_xts(ts_dt(AirPassengers)), "xts")
  expect_s3_class(ts_ts(ts_dt(AirPassengers)), "ts")
  expect_s3_class(ts_df(ts_dt(AirPassengers)), "data.frame")
  expect_s3_class(ts_dt(ts_dt(AirPassengers)), "data.table")
  expect_s3_class(ts_tbl(ts_dt(AirPassengers)), "tbl_df")

  expect_s3_class(ts_xts(ts_tbl(AirPassengers)), "xts")
  expect_s3_class(ts_ts(ts_tbl(AirPassengers)), "ts")
  expect_s3_class(ts_df(ts_tbl(AirPassengers)), "data.frame")
  expect_s3_class(ts_dt(ts_tbl(AirPassengers)), "data.table")
  expect_s3_class(ts_tbl(ts_tbl(AirPassengers)), "tbl_df")

  expect_s3_class(ts_xts(ts_tslist(ts_c(mdeaths, AirPassengers))), "xts")
  expect_s3_class(ts_ts(ts_tslist(ts_c(mdeaths, AirPassengers))), "ts")
  expect_s3_class(ts_df(ts_tslist(ts_c(mdeaths, AirPassengers))), "data.frame")
  expect_s3_class(ts_dt(ts_tslist(ts_c(mdeaths, AirPassengers))), "data.table")
  expect_s3_class(ts_tbl(ts_tslist(ts_c(mdeaths, AirPassengers))), "tbl_df")
})


test_that("conversion between objects works as expected: ldeaths", {

  skip_on_cran()

  x.ts <- ts_c(mdeaths, fdeaths)
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


test_that("conversion between objects works as expected: discoveries", {

  skip_on_cran()

  x.ts <- discoveries
  x.xts <- ts_xts(discoveries)
  x.df <- ts_df(x.xts)
  x.dt <- ts_dt(x.df)
  x.tbl <- ts_tbl(x.dt)

  expect_equal(ts_ts(x.xts), x.ts)
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
  expect_equal(ts_dt(ts_tbl(x.dt)), x.dt)

  expect_equal(ts_dt(ts_ts(x.dt)), x.dt)
  expect_equal(ts_dt(ts_xts(x.dt)), x.dt)
  expect_equal(ts_dt(ts_df(x.dt)), x.dt)
  expect_equal(ts_tbl(ts_dt(x.tbl)), x.tbl)
})


test_that("conversion between objects works as expected: EuStockMarkets", {

  skip_on_cran()

  x.ts <- EuStockMarkets
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



test_that("some trickier situations work properly", {
  skip_on_cran()
  skip_if_not_installed("forecast")
  expect_is(
    ts_bind(
      ts_c(AirPassengers, mdeaths),
      ts_forecast(ts_c(AirPassengers, mdeaths))
    ),
    "ts"
  )

  expect_is(ts_bind(AirPassengers, mdeaths), "ts")
})


test_that("2 colum data.frames work as expected", {
  x <- ts_dt(AirPassengers)
  expect_equal(ts_dt(ts_ts(x)), ts_dt(AirPassengers))
})


test_that("selecting and binding works as expected", {
  dta <- ts_df(ts_c(mdeaths, fdeaths))
  expect_equal(mdeaths, ts_ts(subset(dta, id == "mdeaths", select = -id)))
})



test_that("colname guessing works as expected", {

  # 3 cols
  library(dplyr)
  x.df <- ts_tbl(ts_c(mdeaths, fdeaths)) %>%
    setNames(c("Haha", "Hoho", "Hihi"))

  x.dt <- as.data.table(x.df)
  expect_equal(mdeaths, ts_ts(ts_xts(ts_df(x.df)))[, "mdeaths"])
  expect_equal(mdeaths, ts_ts(ts_df(ts_xts(ts_ts(x.dt))))[, "mdeaths"])

  # 2 cols
  x.df <- ts_tbl(AirPassengers) %>%
    setNames(c("Haha", "Hoho"))

  x.dt <- as.data.table(x.df)
  expect_equal(AirPassengers, ts_ts(ts_xts(ts_df(x.df))))
  expect_equal(AirPassengers, ts_ts(ts_df(ts_xts(ts_ts(x.dt)))))
})



test_that("conversions work with multiple ids", {
  x <- bind_rows(
    mutate(ts_tbl(ts_c(fdeaths, mdeaths)), id2 = "one"),
    mutate(ts_tbl(ts_c(fdeaths, mdeaths)), id2 = "two")
  ) %>%
    ts_df()

  expect_equal(ts_ts(x)[, "fdeaths_two"], fdeaths)
})
