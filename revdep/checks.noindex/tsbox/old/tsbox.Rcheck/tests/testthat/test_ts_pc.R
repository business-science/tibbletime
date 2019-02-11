library(testthat)
library(tsbox)

context("ts_pc")


test_that("colname guessing works as expected", {

  # 3 cols
  library(dplyr)
  x.df <- ts_tbl(ts_c(mdeaths, fdeaths)) %>%
    setNames(c("Haha", "Hoho", "Hihi"))

  expect_equal(ts_pc(mdeaths), ts_ts(ts_xts(ts_df(ts_pc(x.df))))[, "mdeaths"])
  expect_equal(ts_diff(mdeaths), ts_ts(ts_xts(ts_df(ts_diff(x.df))))[, "mdeaths"])
  expect_equal(ts_pcy(mdeaths), ts_ts(ts_xts(ts_df(ts_pcy(x.df))))[, "mdeaths"])
  expect_equal(ts_diffy(mdeaths), ts_ts(ts_xts(ts_df(ts_diffy(x.df))))[, "mdeaths"])

  # 2 cols
  x.df <- ts_tbl(AirPassengers) %>%
    setNames(c("Haha", "Hoho"))

  expect_equal(ts_pc(AirPassengers), ts_ts(ts_xts(ts_df(ts_pc(x.df)))))
  expect_equal(ts_diff(AirPassengers), ts_ts(ts_xts(ts_df(ts_diff(x.df)))))
  expect_equal(ts_pcy(AirPassengers), ts_ts(ts_xts(ts_df(ts_pcy(x.df)))))
  expect_equal(ts_diffy(AirPassengers), ts_ts(ts_xts(ts_df(ts_diffy(x.df)))))
})


test_that("ts_compound, ts_index and ts_pc are consistent", {

  expect_equal(ts_pc(mdeaths), ts_pc(ts_index(mdeaths)))
  expect_equal(ts_pc(AirPassengers), ts_pc(ts_index(AirPassengers)))

  expect_equal(ts_compound(ts_pc(EuStockMarkets)), 
               ts_index(EuStockMarkets))

  expect_equal(ts_compound(ts_pc(ts_c(mdeaths, fdeaths))),
               ts_index(ts_c(mdeaths, fdeaths)))

  expect_equal(ts_pc(ts_compound(ts_pc(ts_c(mdeaths, fdeaths)))),
               ts_pc(ts_index(ts_c(mdeaths, fdeaths))))

  expect_equal(ts_compound(ts_pc(ts_c(AirPassengers))),
               ts_index(ts_c(AirPassengers)))

})


test_that("pc and ts_index works with NA", {
  x0 <- EuStockMarkets
  x0[5:10, ] <- NA
  expect_is(ts_index(ts_pc(x0)), "ts")
})


