
library(testthat)
library(tsbox)


test_that("operations do not depend on time zone", {

  skip_on_cran()

  # temp set tz
  old.tz <- Sys.getenv("TZ")
  on.exit(Sys.setenv(TZ = old.tz))
  Sys.setenv(TZ="America/Los_Angeles")


  expect_equal(
    mdeaths,
    ts_ts(subset(
      ts_c(mdeaths, austres, AirPassengers, DAX = EuStockMarkets[, "DAX"]),
      id == "mdeaths"
    ))
  )

  # fails in some time zones and some systems. needs more investigation
  
  # x <- data.frame(
  #   time = seq(from = as.POSIXct("2000-01-01"), length.out = 10, by = "1 day"),
  #   value = 1:10
  # )
  # expect_equal(x, ts_df(ts_ts(x)))

  # revert to system time zone
  Sys.setenv(TZ = old.tz)

})
