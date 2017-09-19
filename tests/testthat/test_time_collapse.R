context("time_collapse testing")

# Test objects

data(FB)
test_time <- FB
test_tbl_time <- as_tbl_time(test_time, date)

data(FANG)
test_tbl_time_g <- as_tbl_time(FANG, date) %>%
  group_by(symbol)

# Tests

test_that("tbl_time class is retained", {
  test <- time_collapse(test_tbl_time, "yearly")
  expect_is(test,  "tbl_time")
})

test_that("Yearly collapse returns correct rows and cols", {
  test <- time_collapse(test_tbl_time, "yearly")
  expect_equal(ncol(test), 8L)
  expect_equal(nrow(test), 1008L)
})

test_that("Yearly collapse returns correct dates", {
  test <- time_collapse(test_tbl_time, "yearly")
  expect_equal(unique(test$date),
               as.Date(c("2013-12-31", "2014-12-31",
                         "2015-12-31", "2016-12-30")))
})

test_that("Error with non tbl_time object", {
  expect_error(time_collapse(test_time, "yearly"),
               "Object is not of class `tbl_time`.")
})

test_that("Groups are respected", {
  test <- time_collapse(test_tbl_time_g, "yearly")
  expect_is(test, "grouped_df")
  expect_is(test, "grouped_tbl_time")
})

