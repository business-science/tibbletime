library(testthat)
library(tsbox)

context("non standard cnames")

test_that("main functions work with non standard cnames", {

  x <- ts_tbl(ts_c(mdeaths, AirPassengers))
  x2 <- ts_tbl(ts_c(fdeaths, mdeaths))

  names(x) <- c("hey", "heyhey", "Hey")
  names(x2) <- c("hey", "heyhey2", "Hey2")

  expect_is(ts_span(x, start = 1958), "tbl_df")
  expect_is(ts_c(x, x2), "tbl_df")
  expect_is(ts_bind(x, x2), "tbl_df")
  expect_is(ts_scale(x), "tbl_df")
  expect_is(ts_frequency(x, "year"), "tbl_df")
  expect_is(ts_index(x2), "tbl_df")
  expect_is(ts_lag(x), "tbl_df")
  expect_is(ts_pick(x, 'mdeaths'), "tbl_df")

  skip_on_cran()
  expect_is(ts_plot(x), "call")

})

test_that("first object determines col order and col names (#166)", {

  ts1 <- ts_df(mdeaths)
  ts2 <- ts_df(fdeaths)
  colnames(ts1) <- c("time1", "value1")
  colnames(ts2) <- c("time2", "value2")
  ts2 <- ts_span(ts2[, c(2, 1)], end = 1977)
  expect_identical(names(ts_c(ts2, ts1)), c("id", names(ts2)))
  expect_identical(names(ts_bind(ts2, ts1)), names(ts2))
  expect_identical(names(ts_chain(ts2, ts1)), names(ts2))

  expect_identical(names(ts_index(ts2)), names(ts2))
  expect_identical(names(ts_compound(ts2)), names(ts2))
  expect_identical(names(ts_diff(ts2)), names(ts2))
  expect_identical(names(ts_pc(ts2)), names(ts2))
  expect_identical(names(ts_forecast(ts2)), names(ts2))

})


test_that("invalid colnames are handled correctly", {

  x <- ts_tbl(ts_c(mdeaths, AirPassengers))
  x2 <- ts_tbl(ts_c(fdeaths, mdeaths))

  names(x) <- c("Ö oe", "ha ha", "h h~dfsd")
  names(x2) <- c("Ö oe", "ha ha", "h h~dfsd")

  expect_is(ts_span(x, start = 1958), "tbl_df")
  expect_is(ts_c(x, x2), "tbl_df")
  expect_is(ts_bind(x, x2), "tbl_df")
  expect_is(ts_scale(x), "tbl_df")
  expect_is(ts_frequency(x, "year"), "tbl_df")
  expect_is(ts_index(x2), "tbl_df")
  expect_is(ts_lag(x), "tbl_df")
  expect_is(ts_pick(x, 'mdeaths'), "tbl_df")

  skip_on_cran()
  expect_is(ts_plot(x), "call")

})


test_that("years are detected as time", {
  df <- data.frame(year = 2000:2009, value = 1:10)
  expect_equal(ts_summary(ts_ts(df))$end, as.Date("2009-01-01"))
})
