library(testthat)
library(tsbox)

context("ts_chain")

test_that("retropolation gives the correct results", {
  short.ts <- ts_span(mdeaths, start = "1976-01")
  retro <- ts_chain(short.ts, fdeaths)

  # anchor value
  expect_equal(
    ts_span(retro, start = 1976.1, end = 1976.1),
    ts_span(short.ts, start = 1976.1, end = 1976.1)
  )

  # pc rates
  expect_equal(
    ts_span(ts_pc(retro), end = 1976.1),
    ts_span(ts_pc(fdeaths), end = 1976.1)
  )
  expect_equal(
    ts_span(ts_pc(retro), start = 1976.2),
    ts_span(ts_pc(mdeaths), start = 1976.2)
  )

})

