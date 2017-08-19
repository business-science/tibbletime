context("dplyr filter")

# Test objects

test_time <- tibble::tibble(
  date  = c(as.Date("2017-12-01"), as.Date("2017-12-02"), as.Date("2017-12-03")),
  value = c(1, 2, 3)
)

test_tbl_time <- as_tbl_time(test_time, date)

# Tests

test_that("tbl_time class is retained", {
  expect_is(filter(test_tbl_time, date == "2017-12-01"), "tbl_time")
})
