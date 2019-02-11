library(testthat)
library(tsbox)

context("ts_plot")


test_that("ts_plot works", {
  skip_on_cran()

  ts_plot(AirPassengers, title = "AirPassengers", subtitle = "Heyhey")
  tf <- tempfile(fileext = ".pdf")
  ts_save(tf, open = FALSE)
  expect_true(file.size(tf) > 3000)

  tf <- tempfile(fileext = ".png")
  ts_save(tf, open = FALSE)
  expect_true(file.size(tf) > 3000)

  tf <- tempfile(fileext = ".bmp")
  ts_save(tf, open = FALSE)
  expect_true(file.size(tf) > 3000)

  tf <- tempfile(fileext = ".jpeg")
  ts_save(tf, open = FALSE)
  expect_true(file.size(tf) > 3000)

  tf <- tempfile(fileext = ".tiff")
  ts_save(tf, open = TRUE)
  expect_true(file.size(tf) > 3000)

  p <- ts_ggplot(AirPassengers, mdeaths) + theme_tsbox() + scale_color_tsbox()
  expect_is(p, "ggplot")
})
