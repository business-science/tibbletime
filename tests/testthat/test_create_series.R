context("create_series testing")

# Test objects


# Tests

test_that("Can create basic series", {
  
  series <- create_series(~2013-01-01, 1~d)
  check <- as_tbl_time(tibble::tibble(date = as.POSIXct("2013-01-01", tz = "UTC")), date)
  
  expect_equal(series, check)
  expect_is(series, "tbl_time")
})

test_that("Can create series of different classes", {
  expect_equal(create_series(~2013-01-01, 1~d, "Date"),
               as_tbl_time(tibble::tibble(date = as.Date("2013-01-01")), date))
  
  expect_equal(create_series(~2013-01, 1~m, "yearmon"),
               as_tbl_time(tibble::tibble(date = zoo::as.yearmon("2013-01")), date))
  
  expect_equal(create_series(~2013-01, 1~q, "yearqtr"),
               as_tbl_time(tibble::tibble(date = zoo::as.yearqtr("2013-01")), date))
  
  expect_equal(create_series(~1, 1~h, "hms"),
               as_tbl_time(tibble::tibble(date = hms::hms(hours = 1)), date))
})

test_that("Error thrown when creating finer periodicity than allowed", {
  expect_error(create_series(~2013-01-01, 1~h, "Date"),
               "Only year, quarter, month, week, and day periods are allowed for an index of class Date")
  
  expect_error(create_series(~2013-01, 1~d, "yearmon"),
               "Only year, quarter, and month periods are allowed for an index of class yearmon")
  
  expect_error(create_series(~2013-01, 1~d, "yearqtr"),
               "Only year and quarter periods are allowed for an index of class yearqtr")
  
  expect_error(create_series(~1, 1~d, "hms"),
               "Only hour, minute and second periods are allowed for an index of class hms")
})

test_that("Can create vector series", {
  series <- create_series(~2013-01-01, 1~d, as_vector = TRUE)
  check  <- as.POSIXct("2013-01-01", tz = "UTC")
  
  expect_equal(series, check)
})

test_that("Can alter time zone", {
  series <- create_series(~2013-01-01, 1~d, as_vector = TRUE, tz = "EST")
  check  <- as.POSIXct("2013-01-01", tz = "EST")
  
  expect_equal(series, check)
})
