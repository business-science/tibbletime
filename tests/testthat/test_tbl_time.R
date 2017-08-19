context("tbl_time creation")

# Test objects

test_time <- tibble::tibble(
  date  = c(as.Date("2017-12-01"), as.Date("2017-12-02"), as.Date("2017-12-03")),
  value = c(1, 2, 3)
)

test_tbl_time <- as_tbl_time(test_time, date)

# Tests

test_that("tbl_time class is created", {
  expect_is(test_tbl_time, "tbl_time")
})

test_that("tbl_time index is created", {
  expect_true("index" %in% names(attributes(test_tbl_time)))
})

test_that("tbl_time index can be retrieved", {
  expect_equal(retrieve_index(test_tbl_time), dplyr::select(test_time, date))
})

test_that("tbl_time throws error without index", {
  expect_error(as_tbl_time(test_time))
})

test_that("tbl_time throws error with non time index", {
  expect_error(as_tbl_time(test_time, value))
})
