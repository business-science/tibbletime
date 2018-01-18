context("print testing")

# Test objects

test_time <- tibble::tibble(
  date   = c(as.Date("2017-12-01"), as.Date("2017-12-02"), as.Date("2017-12-03")),
  value  = c(1, 2, 3),
  group1 = c("a", "a", "b"),
  group2 = c("d", "e", "e")
)

test_tbl_time   <- as_tbl_time(test_time, date)
test_tbl_time_g <- as_tbl_time(test_time, date) %>%
  dplyr::group_by(group1)

# Tests

test_that("Index is part of the tibble output", {
  expect_output(print.tbl_time(test_tbl_time), "Index: date")
})

test_that("Groups are still printed", {
  expect_output(print.tbl_time(test_tbl_time_g), "Groups: group1")
})

