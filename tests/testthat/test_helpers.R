context("helpers testing")

# Test objects

test_time <- tibble::tibble(
  date   = c(as.Date("2017-12-01"), as.Date("2017-12-02"), as.Date("2017-12-03")),
  value  = c(1, 2, 3),
  group1 = c("a", "a", "b"),
  group2 = c("d", "e", "e")
)

test_tbl_time <- as_tbl_time(test_time, date)

test_time_g <- test_time %>%
  dplyr::group_by(group1)

test_tbl_time_g <- as_tbl_time(test_time_g, date)

# Tests

test_that("Helpers convert to tbl_time", {
  expect_equal(tbl_time(test_time, date), test_tbl_time)
  expect_equal(tbl_time(test_time_g, date), test_tbl_time_g)
})
