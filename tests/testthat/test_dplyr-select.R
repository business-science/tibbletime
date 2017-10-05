context("dplyr select")

# Test objects

test_time <- tibble::tibble(
  date  = c(as.Date("2017-12-01"), as.Date("2017-12-02"), as.Date("2017-12-03")),
  value = c(1, 2, 3)
)

test_tbl_time <- as_tbl_time(test_time, date)

# Tests

test_that("select works as expected", {
  expect_equal(dplyr::select(test_tbl_time, date, value),
               dplyr::select(test_time, date, value))
})

test_that("tbl_time class is retained", {
  expect_is(dplyr::select(test_tbl_time, date, value), "tbl_time")
})

test_that("tbl_time class is lost without date", {
  expect_false("tbl_time" %in% class(dplyr::select(test_tbl_time, value)))
})
