context("dplyr summarise")

# Test objects

test_time <- tibble::tibble(
  date  = c(as.Date("2017-12-01"), as.Date("2017-12-02"), as.Date("2017-12-03")),
  value = c(1, 2, 3)
)

test_tbl_time <- as_tbl_time(test_time, date)

# Tests

test_that("summarise works as expected", {
  expect_equal(dplyr::summarise(test_tbl_time, date = mean(date)),
               dplyr::summarise(test_time, date = mean(date)))
})

test_that("tbl_time class is retained", {
  expect_is(dplyr::summarise(test_tbl_time, date = mean(date)), "tbl_time")
})

test_that("tbl_time class is lost without date", {
  no_tbl_time <- dplyr::summarise(test_tbl_time, value = mean(value))
  expect_false("tbl_time" %in% class(no_tbl_time))
})

