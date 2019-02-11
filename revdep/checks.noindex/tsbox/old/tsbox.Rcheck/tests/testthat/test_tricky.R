library(testthat)
library(tsbox)

context("tricky stuff")

ts_dygraphs(AirPassengers)

test_that("Latest tricky stuff works.", {
  expect_equal(
    mdeaths,
    ts_ts(subset(
      ts_c(mdeaths, austres, AirPassengers, DAX = EuStockMarkets[, "DAX"]),
      id == "mdeaths"
    ))
  )

  # names must be unique!!
  a <- ts_dts(ts_c(AirPassengers, AirPassengers))
  expect_true(length(unique(a[["id"]])) == 2)

  # ts_c for ts objects
  expect_is(ts_c(ts_c(fdeaths, mdeaths), AirPassengers), "ts")
})






test_that("Some trickier stuff works.", {
  expect_s3_class(ts_c(EuStockMarkets, mdeaths, fdeaths), "data.frame")

  x <- ts_c(ts_df(ts_c(mdeaths, fdeaths)), AirPassengers)
  expect_equal(ts_ts(subset(x, id == "AirPassengers")), AirPassengers)

  # series of length 2
  a <- ts_dts(window(AirPassengers, end = c(1949, 2)))
  ts_ts(a)
})



test_that("Irregular regular series work.", {
  expect_s3_class(
    data.frame(
      time = c(
        seq.Date(as.Date("2010-01-01"), by = "day", length.out = 10),
        seq.Date(as.Date("2010-02-01"), by = "day", length.out = 10),
        seq.Date(as.Date("2010-03-01"), by = "day", length.out = 10)
      ),
      value = rnorm(30)
    ) %>% 
      ts_ts(), 
    "ts"
  )
})


test_that("No Invalid .internal.selfref detected.", {
  x <- ts_dts(AirPassengers)
  expect_silent(x[, s := "sdfsd"])
})


test_that("Unordered time works", {
  suppressMessages(library(dplyr))
  ap.rev <- arrange(ts_df(AirPassengers), desc(time))

  expect_equal(ts_ts(ap.rev), AirPassengers)
  expect_equal(ts_ts(ts_diff(ap.rev)), ts_diff(AirPassengers))
})


test_that("Non unique colnames work fine", {

  expect_equal(
    ts_ts(ts_c(mdeaths, fdeaths, ts_df(ts_c(mdeaths, fdeaths)))),
    ts_c(mdeaths, fdeaths, ts_c(mdeaths, fdeaths))
  )

  expect_equal(
    ts_c(mdeaths, EuStockMarkets, ts_tbl(ts_c(mdeaths, EuStockMarkets))),
    ts_tbl(ts_c(mdeaths, EuStockMarkets, ts_c(mdeaths, EuStockMarkets)))
  )

  expect_equal(
    ts_ts(ts_c(mdeaths, mdeaths = ts_df(ts_c(mdeaths)))),
    ts_c(mdeaths, mdeaths = ts_c(mdeaths))
  )

})


test_that("Only combined ids are unique", {

  # individual ids columns don't matter
  df1 <- df2 <- ts_df(ts_c(mdeaths, fdeaths)) 
  df1$cat <- "1"
  df2$cat <- "2"
  comb <- ts_c(df1, df2)
  expect_equal(unique(comb$id), c("mdeaths", "fdeaths"))

  df1 <- df2 <- ts_df(ts_c(mdeaths, mdeaths, fdeaths)) 
  df1$cat <- "1"
  df2$cat <- "2"
  comb <- ts_c(df1, df2)
  expect_equal(unique(comb$id), c("mdeaths", "mdeaths.1", "fdeaths"))

  df1 <- df2 <- ts_df(ts_c(mdeaths, fdeaths))
  df1$cat <- "1"
  df2$cat <- "1"
  comb <- ts_c(df1, df2)
  expect_equal(unique(comb$cat), c("1", "1.1"))
})


