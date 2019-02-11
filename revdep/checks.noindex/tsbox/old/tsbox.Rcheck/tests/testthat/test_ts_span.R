library(testthat)
library(tsbox)

context("ts_span")

test_that("ts_span does the same as base window", {
  ts_win2 <- ts_(window)
  expect_equal(
    ts_win2(ts_c(mdeaths, fdeaths), start = c(1978, 3)),
    ts_span(ts_c(mdeaths, fdeaths), start = "1978-03-01")
  )

  expect_equal(
    ts_win2(ts_c(austres), start = c(1978, 2)),
    ts_span(ts_c(austres), start = "1978-04-01")
  )

  expect_equal(
    ts_win2(ts_c(mdeaths, fdeaths), end = c(1978, 3)),
    ts_span(ts_c(mdeaths, fdeaths), end = "1978-03-01")
  )

  expect_equal(
    ts_win2(ts_c(austres), end = c(1978, 2)),
    ts_span(ts_c(austres), end = "1978-04-01")
  )

  expect_error(
    ts_span(ts_c(austres), start = "1978-06-01", end = "1978-04-01")
  )

  x <- ts_df(austres)

  expect_equal(x[1, 1], ts_start(austres))
  expect_equal(x[nrow(x), 1], ts_end(austres))

})
