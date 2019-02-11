
context("README.md")



test_that("examples from README.md work properly", {

  skip_on_cran()
  
  x.ts <- ts_c(mdeaths, fdeaths)
  x.xts <- ts_xts(x.ts)
  x.df <- ts_df(x.xts)
  x.dt <- ts_dt(x.df)
  x.tbl <- ts_tbl(x.dt)

  ts_scale(x.ts) # normalization
  ts_scale(x.xts)
  ts_scale(x.df)
  ts_scale(x.dt)
  ts_scale(x.tbl)

  ts_trend(x.ts) # loess trend line
  ts_pc(x.xts)
  ts_pcy(x.df)
  ts_lag(x.dt)

  # with external packages
  ts_forecast(x.tbl) # ets forecast

  # collect time series as multiple time series
  ts_c(ts_dt(EuStockMarkets), AirPassengers)
  ts_c(EuStockMarkets, mdeaths)

  # combine time series to a new, single time series
  ts_bind(ts_dt(mdeaths), AirPassengers)
  ts_bind(ts_xts(AirPassengers), ts_tbl(mdeaths))


  ts_df(ts_c(fdeaths, mdeaths))

  ts_plot(ts_scale(ts_c(mdeaths, austres, AirPassengers, DAX = EuStockMarkets[, "DAX"])))
  ts_ggplot(ts_scale(ts_c(mdeaths, austres, AirPassengers, DAX = EuStockMarkets[, "DAX"])))


  ts_(diff)(AirPassengers)
  ts_(rowSums)(ts_c(mdeaths, fdeaths))

  ts_prcomp <- ts_(function(x) predict(prcomp(x, scale = TRUE)))
  ts_prcomp(ts_c(mdeaths, fdeaths))

  ts_dygraphs <- ts_(dygraphs::dygraph, class = "xts")
  ts_forecast <- ts_(function(x) forecast::forecast(x)$mean, vectorize = TRUE)
  ts_seas <- ts_(function(x) seasonal::final(seasonal::seas(x)), vectorize = TRUE)

  ts_dygraphs(ts_c(mdeaths, EuStockMarkets))
  ts_forecast(ts_c(mdeaths, fdeaths))
  ts_seas(ts_c(mdeaths, fdeaths))

  library(dplyr)
  library(tsbox)

  ts_tbl(ts_c(mdeaths, fdeaths)) %>%
    ts_seas() %>%
    ts_plot()



  dta <- ts_df(ts_c(mdeaths, fdeaths))
  expect_is(dta, "data.frame")
})
