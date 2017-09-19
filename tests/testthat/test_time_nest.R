context("time_nest testing")

# Test objects

data(FB)
test_time <- FB
test_tbl_time <- as_tbl_time(test_time, date)

data(FANG)
test_tbl_time_g <- as_tbl_time(FANG, date) %>%
  group_by(symbol)

# Tests

test_that("tbl_time class is retained", {
  test <- time_nest(test_tbl_time, "yearly", -symbol)
  expect_is(test,  "tbl_time")
})

test_that("Renaming with a .key character works", {
  test <- time_nest(test_tbl_time, "yearly", -symbol, .key = "newkey")
  expect_match(colnames(test),  "newkey", all = FALSE)
})

test_that("Renaming with a .key symbol works", {
  test <- time_nest(test_tbl_time, "yearly", -symbol, .key = newkey)
  expect_match(colnames(test),  "newkey", all = FALSE)
})

test_that("time_nest is nested correctly", {
  test <- time_nest(test_tbl_time, "yearly")
  expect_equal(nrow(test),  4L)
  expect_equal(nrow(test$data[[1]]), 252)
})

test_that("Error with non tbl_time object", {
  expect_error(time_nest(test_time, "yearly"),
               "Object is not of class `tbl_time`.")
})

test_that("Groups are respected", {
  test <- time_nest(test_tbl_time_g, "yearly")
  expect_equal(nrow(test), 16L)
  expect_equal(ncol(test), 3L)
  expect_equal(nrow(test$data[[1]]), 252)
})
