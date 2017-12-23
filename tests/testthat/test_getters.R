context("getters testing")

# Test objects

test_time <- tibble::tibble(
  date   = c(as.Date("2017-12-01"), as.Date("2017-12-02"), as.Date("2017-12-03")),
  value  = c(1, 2, 3),
  group1 = c("a", "a", "b"),
  group2 = c("d", "e", "e")
)

test_tbl_time <- as_tbl_time(test_time, date)

# Tests

test_that("Index getters are working", {
  expect_equal(get_index_quo(test_tbl_time), rlang::quo(date))
  expect_equal(get_index_char(test_tbl_time), "date")
  expect_equal(get_index_col(test_tbl_time), test_tbl_time$date)
  expect_equal(get_.index_col(test_tbl_time), to_posixct_numeric(test_tbl_time$date))
  expect_equal(get_index_time_zone(test_tbl_time), "UTC")
  expect_equal(get_index_class(test_tbl_time), "Date")
  expect_equal(get_index_dispatcher(test_tbl_time), structure(list(), class = "Date"))
})

test_that("Creation getters are working", {
  expect_equal(get_default_time_zone(), "UTC")
  expect_equal(get_index_col_time_zone(test_tbl_time), "UTC")
  expect_equal(get_index_col_class(test_tbl_time$date), "Date")
})
