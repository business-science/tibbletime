context("time_filter testing")

# Test objects

data(FB)
test_time <- FB
test_tbl_time <- as_tbl_time(test_time, date)

data(FANG)
test_tbl_time_g <- as_tbl_time(FANG, date) %>%
  dplyr::group_by(symbol)

# Tests

test_that("tbl_time class is retained", {
  test <- time_filter(test_tbl_time, ~2013)
  expect_is(test,  "tbl_time")
})

test_that("Filtering is expanded correctly", {
  test <- time_filter(test_tbl_time, ~2013)
  test_filter <- dplyr::filter(test_tbl_time,
                               date >= "2013-01-01",
                               date <= "2013-12-31")
  expect_equal(test, test_filter)
})

test_that("Filtering is expanded correctly - double sided", {
  test <- time_filter(test_tbl_time, 2013-01 ~ 2014-02)
  test_filter <- dplyr::filter(test_tbl_time,
                               date >= "2013-01-01",
                               date <= "2014-02-28")
  expect_equal(test, test_filter)
})

test_that("Time filter works with POSIXct objects", {
  test_tbl_time_posix <- dplyr::mutate(test_tbl_time, date = as.POSIXct(date))
  test <- time_filter(test_tbl_time_posix,
                      2013-01-02 + 12:00:00 ~ 2014-02-01 + 14:01:01)
  test_filter <- dplyr::filter(test_tbl_time,
                               date >= "2013-01-03",
                               date <= "2014-01-31") %>%
    dplyr::mutate(date = as.POSIXct(date))
  expect_equal(test, test_filter)
})

test_that("Error with non tbl_time object", {
  expect_error(time_filter(test_time, ~2013),
               "Object is not of class `tbl_time`.")
})

test_that("Groups are respected", {
  test <- time_filter(test_tbl_time_g, ~2013)
  expect_equal(nrow(test), 1008L)
})

test_that("Time filter subsetting [~i] works", {
  expect_equal(ncol(test_tbl_time[~2013]), 8L)
  expect_equal(nrow(test_tbl_time[~2013]), 252L)
})

test_that("Column subsetting [i] works", {
  expect_equal(ncol(test_tbl_time[2]), 1L)
  expect_equal(nrow(test_tbl_time[2]), 1008L)
})

test_that("Column subsetting [i, drop] works", {
  expect_equal(ncol(test_tbl_time[2, drop = FALSE]), 1L)
  expect_equal(nrow(test_tbl_time[2, drop = FALSE]), 1008L)
})

test_that("Row subsetting [i,] works", {
  expect_equal(ncol(test_tbl_time[2,]), 8L)
  expect_equal(nrow(test_tbl_time[2,]), 1L)
})

test_that("Row subsetting [i, , drop = FALSE] works", {
  expect_equal(ncol(test_tbl_time[2, , drop = FALSE]), 8L)
  expect_equal(nrow(test_tbl_time[2, , drop = FALSE]), 1L)
})

test_that("Row subsetting [i, j] works", {
  expect_equal(ncol(test_tbl_time[1, 2]), 1L)
  expect_equal(nrow(test_tbl_time[2, 2]), 1L)
})
