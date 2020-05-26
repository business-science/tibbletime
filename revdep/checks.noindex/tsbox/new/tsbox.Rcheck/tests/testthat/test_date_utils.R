library(testthat)
library(tsbox)

context("date utils")


test_that("df aggregation using date_ functions is working", {

  # 3 cols
  library(dplyr)
  x <- ts_tbl(ts_c(mdeaths, fdeaths)) %>%
    mutate(time = date_year(time)) %>%
    group_by(id, time) %>%
    summarize(value = mean(value)) %>%
    ungroup()

  expect_equal(x, arrange(ts_tbl(ts_frequency(ts_c(mdeaths, fdeaths), "year")), id))


  x <- ts_tbl(ts_c(mdeaths, fdeaths)) %>%
    mutate(time = date_quarter(time)) %>%
    group_by(id, time) %>%
    summarize(value = mean(value)) %>%
    ungroup()

  expect_equal(x, arrange(ts_tbl(ts_frequency(ts_c(mdeaths, fdeaths), "quarter")), id))

  x <- ts_bind(NA, ts_tbl(ts_c(EuStockMarkets)), NA) %>%
    mutate(time = as.Date(date_month(time))) %>%
    group_by(id, time) %>%
    summarize(value = mean(value)) %>%
    ungroup() %>%
    ts_na_omit()
  expect_equal(x, arrange(ts_tbl(ts_frequency(ts_c(EuStockMarkets), "month")), id))

  # include incompletes
  x <- ts_tbl(ts_c(EuStockMarkets)) %>%
    mutate(time = as.Date(date_month(time))) %>%
    group_by(id, time) %>%
    summarize(value = mean(value)) %>%
    ungroup()
  expect_equal(x, arrange(ts_tbl(ts_frequency(ts_c(EuStockMarkets), "month", na.rm = TRUE)), id))

})


test_that("time_shift is working", {
  x <- ts_tbl(ts_c(mdeaths, fdeaths))
  expect_equal(x$time, tsbox:::time_shift(x$time))

  x1 <- ts_tbl(ts_c(mdeaths, fdeaths)) %>%
    mutate(time = tsbox:::time_shift(time, by = "month"))
  xlag <- ts_lag(x)

  expect_equal(xlag, x1)
})



test_that("non heuristic reguarization works for Date", {

  x <- as.Date(c("2001-01-02", "2001-01-04", "2001-01-06", "2001-01-08",
      "2001-01-10", "2001-01-14"))
  expect_is(regularize_non_heuristic(x), "Date")
})

test_that("time shift works in special situations", {

  z <- time_shift(c(seq(as.POSIXct("2001-01-01"), as.POSIXct("2001-01-02"), by = "hour"),
    as.POSIXct("2001-01-02 00:02:11 CET")), by = "hour")
  expect_is(z, "POSIXct")
})


# test_that("time zones are not removed", {
#   x <- ts_tbl(EuStockMarkets)
#   attr(x$time, "tzone") <- "UTC"

#   # ts_pc(x)

# })

