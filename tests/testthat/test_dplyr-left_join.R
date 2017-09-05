context("dplyr left_join")

# Test objects

test_time <- tibble::tibble(
  date  = c(as.Date("2017-12-01"), as.Date("2017-12-01"), as.Date("2017-12-03")),
  value = c(1, 2, 3)
)

test_time_2 <- tibble::tibble(
  date  = c(as.Date("2017-12-01"), as.Date("2017-12-01"), as.Date("2017-12-03")),
  value = c(2, 3, 4)
)

test_tbl_time <- as_tbl_time(test_time, date)
test_tbl_time_2 <- as_tbl_time(test_time_2, date)


# Tests

test_that("left_join works as expected", {
  expect_equal(left_join(test_tbl_time, test_tbl_time_2),
               left_join(test_time, test_time_2))
})

test_that("tbl_time class is retained if at least the left side is tbl_time", {
  expect_is(left_join(test_tbl_time, test_tbl_time_2), "tbl_time")
  expect_is(left_join(test_tbl_time, test_time_2),     "tbl_time")
})

test_that("tbl_time class is lost if the left side is not tbl_time", {
  expect_false("tbl_time" %in% class(left_join(test_time_2, test_tbl_time)))
})
