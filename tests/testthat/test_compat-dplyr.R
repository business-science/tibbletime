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

test_that("rename() catches a renamed index and returns tibble", {
  now_a_tbl <- dplyr::rename(test_tbl_time, date2 = date)
  expect_equal(class(now_a_tbl), c("tbl_df", "tbl", "data.frame"))
})
