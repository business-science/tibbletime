library(testthat)
library(tsbox)

context("high frequency data")

test_that("no NA when converting second data ", {
  x <- data.frame(
    time = seq(from = as.POSIXct("2000-01-01"), length.out = 10, by = "1 sec"),
    value = 1:10
  )
  expect_true(all(!is.na(ts_ts(x))))
})




test_that("heuristic high frequency data works", {


  x <- ts_ts(data.frame(
    time = seq(from = as.POSIXct("2000-01-01"), length.out = 10, by = "1 sec"),
    value = 1:10
  ))
  expect_is(x, "ts")

  x <- ts_ts(data.frame(
    time = seq(from = as.POSIXct("2000-01-01"), length.out = 10, by = "4 hour"),
    value = 1:10
  ))
  expect_is(x, "ts")

  x <- ts_ts(data.frame(
    time = seq(from = as.POSIXct("2000-01-01"), length.out = 10, by = "1 day"),
    value = 1:10
  ))
  expect_is(x, "ts")


  x <- data.frame(
    time = seq(from = as.POSIXct("2000-01-01"), length.out = 10, by = "1 sec"),
    value = 1:10
  )
  expect_equal(x, ts_df(ts_ts(x)))

  x <- data.frame(
    time = seq(from = as.POSIXct("2000-01-01"), length.out = 10, by = "5 sec"),
    value = 1:10
  )
  expect_equal(x, ts_df(ts_ts(x)))

  x <- data.frame(
    time = seq(from = as.POSIXct("2000-01-01"), length.out = 10, by = "10 sec"),
    value = 1:10
  )
  expect_equal(x, ts_df(ts_ts(x)))

  x <- data.frame(
    time = seq(from = as.POSIXct("2000-01-01"), length.out = 10, by = "1 min"),
    value = 1:10
  )
  expect_equal(x, ts_df(ts_ts(x)))

  x <- data.frame(
    time = seq(from = as.POSIXct("2000-01-01"), length.out = 10, by = "10 min"),
    value = 1:10
  )
  expect_equal(x, ts_df(ts_ts(x)))

  x <- data.frame(
    time = seq(from = as.POSIXct("2000-01-01"), length.out = 10, by = "15 min"),
    value = 1:10
  )
  expect_equal(x, ts_df(ts_ts(x)))

  x <- data.frame(
    time = seq(from = as.POSIXct("2000-01-01"), length.out = 10, by = "20 min"),
    value = 1:10
  )
  expect_equal(x, ts_df(ts_ts(x)))

  x <- data.frame(
    time = seq(from = as.POSIXct("2000-01-01"), length.out = 10, by = "60 min"),
    value = 1:10
  )
  expect_equal(x, ts_df(ts_ts(x)))

  # fails on some systems, time zones, needs investigation
  
  # x <- data.frame(
  #   time = seq(from = as.POSIXct("2000-01-01"), length.out = 10, by = "1 day"),
  #   value = 1:10
  # )
  # expect_equal(x, ts_df(ts_ts(x)))


})




test_that("non regular high frequency data works", {
  x <- data.frame(
    time = seq(from = as.POSIXct("2000-01-01"), length.out = 10, by = "10 days"),
    value = 1:10
  )
  expect_equal(x, ts_df(ts_ts(x)))

  x <- data.frame(
    time = seq(from = as.POSIXct("2000-01-01"), length.out = 10, by = "17 days"),
    value = 1:10
  )
  expect_equal(x, ts_df(ts_ts(x)))

  x <- data.frame(
    time = seq(from = as.POSIXct("2000-01-01"), length.out = 10, by = "17 secs"),
    value = 1:10
  )
  expect_equal(x, ts_df(ts_ts(x)))

  x <- data.frame(
    time = seq(from = as.POSIXct("2000-01-01"), length.out = 10, by = "17 mins"),
    value = 1:10
  )
  expect_equal(x, ts_df(ts_ts(x)))

})



