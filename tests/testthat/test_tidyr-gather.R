context("tidyr gather")

# Test objects

test_time <- tibble::tibble(
  date  = c(as.Date("2017-12-01"), as.Date("2017-12-02"), as.Date("2017-12-03")),
  value1 = c(1, 2, 3),
  value2 = c(2, 3, 4)
)

test_tbl_time <- as_tbl_time(test_time, date)

# Tests

test_that("gather works as expected", {
  expect_equal(gather(test_tbl_time, key = value_type, value = value, -date),
               gather(test_time,     key = value_type, value = value, -date))
})

test_that("tbl_time class is retained", {
  expect_is(gather(test_tbl_time, key = value_type, value = value, -date), "tbl_time")
})

test_that("tbl_time class is lost without date", {
  suppressWarnings(expect_false("tbl_time" %in% class(gather(test_tbl_time, key = value_type, value = value))))
})
