context("dplyr group_by")

# Test objects

test_time <- tibble::tibble(
  group = c("g1", "g1", "g2"),
  date  = c(as.Date("2017-12-01"), as.Date("2017-12-02"), as.Date("2017-12-03")),
  value = c(1, 2, 3)
)

test_tbl_time <- as_tbl_time(test_time, date)
grouped_test  <- dplyr::group_by(test_tbl_time, group)

# Tests

test_that("tbl_time class is retained", {
  expect_is(grouped_test, "tbl_time")
})

test_that("grouped_tbl_time class is added", {
  expect_is(grouped_test, "grouped_tbl_time")
})

test_that("grouped_tbl_time class is retained when grouping on a grouped_tbl_time dataframe", {
  expect_is(dplyr::group_by(grouped_test, group), "grouped_tbl_time")
})

test_that("grouped_tbl_time and tbl_time classes are before grouped_df and tbl_df", {
  grouped_tbl_time_order <- which(class(grouped_test) == "grouped_tbl_time")
  tbl_time_order         <- which(class(grouped_test) == "tbl_time")
  grouped_df_order       <- which(class(grouped_test) == "grouped_df")
  tbl_df_order           <- which(class(grouped_test) == "tbl_df")

  expect_true(grouped_tbl_time_order < grouped_df_order)
  expect_true(tbl_time_order         < tbl_df_order)
})
