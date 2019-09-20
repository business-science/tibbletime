library(testthat)
library(tsbox)

# install.packages(c("tsibble", "xts", "timeSeries", "zoo", "tibbletime", "tseries"))

context("automated tests for all supported classes")


test_that("two way conversion", {

  skip_if_not_installed("tibbletime")
  skip_if_not_installed("tsibble")
  skip_if_not_installed("timeSeries")
  skip_if_not_installed("zoo")
  skip_if_not_installed("tis")

  for (class in names(tsbox:::supported_classes())){
    message(class)

    ts_fun <- get(paste0("ts_", class))

    # single series
    expect_equal(ts_ts(ts_fun(AirPassengers)), AirPassengers)

    # tsibble alphabetically reorders key column, separate test below
    if (class == "tsibble") next

    # tis: does not deal correctly with 'as.tis(EuStockMarkets)'
    # timeSeries: stored in seconds only, which prevents back covnersion to ts
    if (!(class %in% c("timeSeries", "tis", "irts"))){
      expect_equal(ts_ts(ts_fun(EuStockMarkets)), EuStockMarkets)
    }

    # mixed frequencies
    expect_equal(ts_ts(ts_fun(ts_c(austres, AirPassengers))), ts_c(austres, AirPassengers))
    # non alphabetical order, multi series
    expect_equal(ts_ts(ts_fun(ts_c(mdeaths, fdeaths))), ts_c(mdeaths, fdeaths))
    # non alphabetical order, multi series
    expect_equal(ts_ts(ts_fun(ts_c(mdeaths, AirPassengers))), ts_c(mdeaths, AirPassengers))

  }

})


