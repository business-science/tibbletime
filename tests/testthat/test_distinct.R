context("dplyr distinct")

# Test objects

test_time <- tibble::tibble(
  date  = c(as.Date("2017-12-01"), as.Date("2017-12-02"), as.Date("2017-12-03")),
  value = c(1, 2, 3)
)

test_tbl_time <- as_tbl_time(test_time, date)

# Tests

test_that("tbl_time class is retained", {
  expect_is(distinct(test_tbl_time), "tbl_time")
})

test_that("tbl_time class is lost", {
  no_tbl_time <- distinct(test_tbl_time, value, .keep_all = FALSE)
  expect_false("tbl_time" %in% class(no_tbl_time))
})
