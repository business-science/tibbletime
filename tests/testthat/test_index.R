context("index testing")

# Test objects

test_time <- tibble::tibble(
  date   = c(as.Date("2017-12-01"), as.Date("2017-12-02"), as.Date("2017-12-03")),
  value  = c(1, 2, 3),
  group1 = c("a", "a", "b"),
  group2 = c("d", "e", "e")
)

test_tbl_time <- as_tbl_time(test_time, date)

# Tests

test_that("tbl_time index can be retrieved", {
  expect_equal(retrieve_index(test_tbl_time), dplyr::select(test_time, date))
})

test_that("tbl_time index can be retrieved with one group", {
  test_tbl_time <- dplyr::group_by(test_tbl_time, group1)
  expect_equal(retrieve_index(test_tbl_time), dplyr::select(test_time, group1, date))
})

test_that("tbl_time index can be retrieved with multiple groups", {
  test_tbl_time <- dplyr::group_by(test_tbl_time, group1, group2)
  expect_equal(retrieve_index(test_tbl_time), dplyr::select(test_time, group1, group2, date))
})
