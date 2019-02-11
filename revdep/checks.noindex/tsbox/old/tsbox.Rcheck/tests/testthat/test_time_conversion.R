library(testthat)
library(data.table)
library(tsbox)
library(dplyr)
library(tsbox)

context("data time conversion")

test_that("two way date time conversion (non heuristic)", {
  expect_equal(tsbox:::POSIXct_to_tsp(tsbox:::ts_to_POSIXct(AirPassengers)), tsp(AirPassengers))
  expect_equal(tsbox:::POSIXct_to_tsp(tsbox:::ts_to_POSIXct(EuStockMarkets)), tsp(EuStockMarkets))
  expect_equal(tsbox:::POSIXct_to_tsp(tsbox:::ts_to_POSIXct(discoveries)), tsp(discoveries))
  expect_equal(tsbox:::POSIXct_to_tsp(tsbox:::ts_to_POSIXct(mdeaths)), tsp(mdeaths))
  expect_equal(tsbox:::POSIXct_to_tsp(tsbox:::ts_to_POSIXct(uspop)), tsp(uspop))
  expect_equal(tsbox:::POSIXct_to_tsp(tsbox:::ts_to_POSIXct(austres)), tsp(austres))
})

test_that("two way date time conversion (heuristic)", {
  expect_equal(tsbox:::date_time_to_tsp(tsbox:::ts_to_date_time(AirPassengers)), tsp(AirPassengers))
  expect_equal(tsbox:::date_time_to_tsp(tsbox:::ts_to_date_time(EuStockMarkets)), tsp(EuStockMarkets))
  expect_equal(tsbox:::date_time_to_tsp(tsbox:::ts_to_date_time(discoveries)), tsp(discoveries))
  expect_equal(tsbox:::date_time_to_tsp(tsbox:::ts_to_date_time(mdeaths)), tsp(mdeaths))
  expect_equal(tsbox:::date_time_to_tsp(tsbox:::ts_to_date_time(uspop)), tsp(uspop))
  expect_equal(tsbox:::date_time_to_tsp(tsbox:::ts_to_date_time(austres)), tsp(austres))
})
