library(testthat)
library(tsbox)

context("ts_")


test_that("ts_ works with more exotic options", {
  skip_if_not_installed("dygraphs")

  expect_equal(ts_(rowSums)(ts_c(mdeaths, fdeaths)), mdeaths + fdeaths)
  ts_(dygraphs::dygraph, class = "xts")

  expect_equal(
    ts_(function(x) x, class = "ts", vectorize = TRUE)(ts_c(mdeaths, fdeaths)),
    ts_c(mdeaths, fdeaths)
  )


  expect_error(load_suggested("blabla"))
  expect_error(ts_(function(x) x, reclass = FALSE, vectorize = TRUE))
})
