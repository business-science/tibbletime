context("dplyr ungroup")

# Test objects

test_time <- tibble::tibble(
  group = c("g1", "g1", "g2"),
  date  = c(as.Date("2017-12-01"), as.Date("2017-12-02"), as.Date("2017-12-03")),
  value = c(1, 2, 3)
)

test_tbl_time <- as_tbl_time(test_time, date)
grouped_test  <- group_by(test_tbl_time, group)
grouped_test_time  <- group_by(test_time, group)

# Tests

test_that("ungroup works as expected", {
  expect_equal(ungroup(grouped_test),
               ungroup(grouped_test_time))
})

test_that("tbl_time class is retained", {
  expect_is(ungroup(test_tbl_time), "tbl_time")
})

test_that("grouped_tbl_time class is lost", {
  expect_false("grouped_tbl_time" %in% class(ungroup(test_tbl_time)))
})
