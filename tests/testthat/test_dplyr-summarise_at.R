context("dplyr summarise_at")

# Test objects

test_time <- tibble::tibble(
  date  = c(as.Date("2017-12-01"), as.Date("2017-12-02"), as.Date("2017-12-03")),
  value = c(1, 2, 3)
)

test_tbl_time <- as_tbl_time(test_time, date)

# Tests

test_that("summarise_at works as expected", {
  expect_equal(summarise_at(test_tbl_time, .vars = dplyr::vars(date, value), .funs = mean),
               summarise_at(test_time, .vars = dplyr::vars(date, value), .funs = mean))
})

test_that("tbl_time class is retained", {
  expect_is(summarise_at(test_tbl_time, .vars = dplyr::vars(date, value), .funs = mean), "tbl_time")
})

test_that("tbl_time class is lost without date", {
  expect_false("tbl_time" %in% class(summarise_at(test_tbl_time, .vars = dplyr::vars(value), .funs = mean)))
})
