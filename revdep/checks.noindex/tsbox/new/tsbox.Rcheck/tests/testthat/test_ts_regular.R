library(testthat)
library(tsbox)


context("ts_regular")

test_that("conversion produces right classes", {
  x0 <- AirPassengers
  x0[c(10, 15)] <- NA
  x <- ts_na_omit(ts_dts(x0))
  expect_equal(ts_ts(ts_regular(x)), x0)

  m <- mdeaths
  m[c(10, 69)] <- NA
  f <- fdeaths
  f[c(1, 3, 15)] <- NA

  comb <- ts_ts(ts_regular(ts_na_omit(ts_dts(ts_c(f, m)))))
  expect_equal(comb, cbind(f, m))
})



test_that("handles, regular, non standard series correctly", {
  expect_equal(EuStockMarkets, ts_regular(EuStockMarkets))

  expect_error(
    ts_regular(data.frame(
      time = as.Date(c("2001-01-01", "2002-01-01", "2010-06-01")),
      value = 1
    ))
  )
})


test_that("does not change colnames in non standard order", {

  x <- ts_df(ts_c(mdeaths, fdeaths))
  setcolorder(x, c("time", "id", "value"))
  expect_equal(ts_regular(x), x)

})
