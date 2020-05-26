context("as_period testing")

# Test objects

data(FB)
test_time <- FB
test_tbl_time <- as_tbl_time(test_time, date)

data(FANG)
test_tbl_time_g <- as_tbl_time(FANG, date) %>%
  group_by(symbol)

# Tests

test_that("Converting to more granular throws error", {
  expect_error(as_period(test_tbl_time, "hourly"))
})

test_that("Can convert to monthly", {
  test_period <- as_period(test_tbl_time, "monthly")
  expect_equal(nrow(test_period), 48L)
  expect_equal(ncol(test_period), 8L)
  expect_equal(test_period$date[2], as.Date("2013-02-01"))
})

test_that("Can convert to monthly - end", {
  test_period <- as_period(test_tbl_time, "monthly", side = "end")
  expect_equal(nrow(test_period), 48L)
  expect_equal(ncol(test_period), 8L)
  expect_equal(test_period$date[2], as.Date("2013-02-28"))
})

test_that("Can convert to yearly", {
  test_period <- as_period(test_tbl_time, "yearly")
  expect_equal(nrow(test_period), 4L)
  expect_equal(ncol(test_period), 8L)
  expect_equal(test_period$date[2], as.Date("2014-01-02"))
})

test_that("Can convert to yearly - end", {
  test_period <- as_period(test_tbl_time, "yearly", side = "end")
  expect_equal(nrow(test_period), 4L)
  expect_equal(ncol(test_period), 8L)
  expect_equal(test_period$date[2], as.Date("2014-12-31"))
})

test_that("Include endpoints with side = 'start' includes last point", {
  start <- as_period(test_tbl_time, "yearly", include_endpoints = TRUE)

  expect_equal(
    object = start$date[length(start$date)],
    expected = as.Date("2016-12-30"))
})

test_that("Include endpoints with side = 'start' includes last point", {
  end <- as_period(test_tbl_time, "yearly",
                     side = "end", include_endpoints = TRUE)

  expect_equal(
    object = end$date[1],
    expected = as.Date("2013-01-02"))
})

test_that("Error with non tbl_time object", {
  expect_error(as_period(test_time, "yearly"),
               "Object is not of class `tbl_time`.")
})

test_that("Groups are respected", {
  test_period <- as_period(test_tbl_time_g, "yearly")
  expect_equal(nrow(test_period), 16L)
  expect_equal(ncol(test_period), 8L)
})
