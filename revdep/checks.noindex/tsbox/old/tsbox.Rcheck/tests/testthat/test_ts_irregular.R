library(testthat)
library(tsbox)

context("true irregular series")

test_that("deals with true irregular series", {

  x <- data.frame(
    time = as.POSIXct(c("2000-01-01", "2001-01-01", "2005-03-03", "2007-03-03", "2007-03-05", "2007-03-09", "2007-05-03", "2007-09-03")),
    value = 1:8
  )

  expect_error(ts_ts(x))
  expect_equal(x, ts_df(ts_tbl(x)))


  x <- data.frame(
    time = as.Date(c("2000-01-01", "2001-01-01", "2005-03-03", "2007-03-03")),
    value = 1:4
  )

  expect_error(ts_ts(x))
  expect_equal(x, ts_df(ts_tbl(x)))


})


test_that("universal functions work with irregular series", {


  x <- data.frame(
    time = as.Date(c("2000-01-01", "2001-01-01", "2005-03-03", "2007-03-03", "2007-03-05", "2007-03-09", "2007-05-03", "2007-09-03")),
    value = 1:8
  )
  expect_error(ts_ts(x))

  expect_is(ts_c(x, ts_trend(x)), "data.frame")
  expect_is(ts_c(x, ts_index(x, base = "2007-03-03")), "data.frame")

  # Should work, but does not
  # expect_is(ts_c(x, ts_scale(x)), "data.frame")

})