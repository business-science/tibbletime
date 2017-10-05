context("dplyr mutate")

# Test objects

test_time <- tibble::tibble(
  date  = c(as.Date("2017-12-01"), as.Date("2017-12-02"), as.Date("2017-12-03")),
  value = c(1, 2, 3)
)

test_tbl_time <- as_tbl_time(test_time, date)

# Tests

test_that("mutate works as expected", {
  expect_equal(dplyr::mutate(test_tbl_time, x = 4),
               dplyr::mutate(test_time, x = 4))
})

test_that("transmute works as expected", {
  expect_equal(dplyr::transmute(test_tbl_time, date = date, x = 4),
               dplyr::transmute(test_time, date = date, x = 4))
})

test_that("tbl_time class is retained", {
  expect_is(dplyr::mutate(test_tbl_time, new_value = 2 * value), "tbl_time")
})

test_that("tbl_time class is lost on transmute without date", {
  expect_false("tbl_time" %in% class(dplyr::transmute(test_tbl_time, new_value = 2 * value)))
})
