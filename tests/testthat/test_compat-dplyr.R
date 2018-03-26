context("dplyr compatability")

# Test objects

test_time <- tibble::tibble(
  date   = c(as.Date("2017-12-01"), as.Date("2017-12-02"), as.Date("2017-12-03")),
  value  = c(1, 2, 3),
  group1 = c("a", "a", "b"),
  group2 = c("d", "e", "e")
)

test_tbl_time <- as_tbl_time(test_time, date)

# Tests

test_that("rename() catches a renamed index and returns new tbl_time", {
  a_new_tbl_time <- dplyr::rename(test_tbl_time, date2 = date)
  index_char <- get_index_char(a_new_tbl_time)
  expect_equal(class(a_new_tbl_time), c("tbl_time", "tbl_df", "tbl", "data.frame"))
  expect_equal(index_char, "date2")
})

test_that("rename() catches a multiple renamed index and uses the last one", {
  a_new_tbl_time <- dplyr::rename(test_tbl_time, date2 = date, date3 = date)
  index_char <- get_index_char(a_new_tbl_time)
  expect_equal(class(a_new_tbl_time), c("tbl_time", "tbl_df", "tbl", "data.frame"))
  expect_equal(index_char, "date3")
})
