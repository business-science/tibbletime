context("time_summarise testing")

# Test objects

data(FB)
test_time <- FB
test_tbl_time <- as_tbl_time(test_time, date)

data(FANG)
test_tbl_time_g <- as_tbl_time(FANG, date) %>%
  group_by(symbol)

# Tests

test_that("tbl_time class is retained", {
  test <- time_summarise(test_tbl_time, "yearly", x = mean(adjusted))
  expect_is(test,  "tbl_time")
})

test_that("Summarise with 1 condition works", {
  test <- time_summarise(test_tbl_time, "yearly", x = mean(adjusted))
  expect_equal(ncol(test), 2L)
  expect_equal(nrow(test), 4L)
})

test_that("Summarise with 2 conditions works", {
  test <- time_summarise(test_tbl_time, "yearly",
                         x = mean(adjusted), y = sd(adjusted))
  expect_equal(ncol(test), 3L)
  expect_equal(nrow(test), 4L)
})

test_that("Error with non tbl_time object", {
  expect_error(time_summarise(test_time, "yearly", x = mean(adjusted)),
               "Object is not of class `tbl_time`.")
})

test_that("Groups are respected", {
  test <- time_summarise(test_tbl_time_g, "yearly", x = mean(adjusted))
  expect_equal(ncol(test), 3L)
  expect_equal(nrow(test), 16L)
})

test_that("time_summarize calls time_summarise", {
  test <- time_summarize(test_tbl_time_g, "yearly", x = mean(adjusted))
  expect_equal(ncol(test), 3L)
  expect_equal(nrow(test), 16L)
})

