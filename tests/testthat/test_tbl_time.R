context("tbl_time creation")

# Test objects

test_time <- tibble::tibble(
  group = c("g1", "g1", "g2"),
  date  = c(as.Date("2017-12-01"), as.Date("2017-12-02"), as.Date("2017-12-03")),
  value = c(1, 2, 3)
)

test_time_data_frame <- as.data.frame(test_time)
test_tbl_time_base <- tbl_time(test_time, date)
test_tbl_time      <- as_tbl_time(test_time, date)
test_tbl_time_g    <- as_tbl_time(dplyr::group_by(test_time, group), date)

# Tests

test_that("tbl_time class is created", {
  expect_is(test_tbl_time,      "tbl_time")
  expect_is(test_tbl_time_base, "tbl_time")
  expect_is(test_tbl_time_g,    "tbl_time")
})

test_that("data.frame objects can be coerced by default method", {
  test_tbl <- as_tbl_time(test_time_data_frame, date)
  expect_is(test_tbl,      "tbl_time")
})

test_that("grouped_tbl_time class is created when converting a grouped_df", {
  test_tbl_time_g    <- as_tbl_time(dplyr::group_by(test_time, group), date)
  expect_is(test_tbl_time_g, "grouped_tbl_time")
})

test_that("tbl_time index is created", {
  expect_true("index" %in% names(attributes(test_tbl_time)))
})

test_that("as_tbl_time() on a tbl_time object shouldn't repeat classes", {
  test_tbl_time2 <- as_tbl_time(test_tbl_time, date)
  expect_true(length(which(class(test_tbl_time2) == "tbl_time")) == 1)
})

test_that("tbl_time throws error without index", {
  expect_error(as_tbl_time(test_time))
})

test_that("tbl_time throws error with non time index", {
  expect_error(as_tbl_time(test_time, value))
})
