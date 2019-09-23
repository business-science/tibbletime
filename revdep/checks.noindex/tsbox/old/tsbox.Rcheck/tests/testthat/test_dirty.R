library(testthat)
library(tsbox)

context("dirty situations")

test_that("works with df with improper col classes", {

  library(dplyr)
  x.chr <- ts_tbl(mdeaths) %>%
    mutate(time = as.character(time))

  expect_is(ts_ts(x.chr), "ts")

  x.fct <- ts_tbl(mdeaths) %>%
    mutate(time = as.factor(as.character(time)))

  expect_is(ts_ts(x.fct), "ts")

})



test_that("time column of daily data is treated as Date (#114)", {

  x <- tibble(
    time = seq.Date(as.Date("2000-01-01"), length.out = 10, by = "day"),
    value = rnorm(10)
  )

  z <- ts_dts(ts_ts(x))
  expect_is(z$time, "Date")

})


test_that("time column of daily data is survives two way conversion (#137)", {
  x <- structure(list(time = structure(c(16030, 16031, 16034, 16035,
  16036), class = "Date"), value = c(18680.35, 18766.53, 18741.95,
  18759.68, 18812.33)), class = "data.frame", row.names = c(NA,
  -5L))

  z <- ts_na_omit(ts_tbl(ts_ts(x)))
  expect_equal(z$time, x$time)
})
