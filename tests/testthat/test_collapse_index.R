context("collapse_index testing")

# Test objects

test_time <- tibble::tibble(
  date   = c(as.Date("2017-12-01"), as.Date("2017-12-02"), as.Date("2017-12-03")),
  value  = c(1, 2, 3),
  group1 = c("a", "a", "b"),
  group2 = c("d", "e", "e")
)

# Tests

test_that("Yearly collapse returns correct dates", {
  test <- collapse_index(test_time$date, "yearly")
  expect_equal(unique(test),
               as.Date("2017-12-03"))
})

test_that("side = 'start' returns start of period", {
  test <- collapse_index(test_time$date, "yearly", side = "start")
  expect_equal(unique(test),
               as.Date("2017-12-01"))
})

test_that("start_date can alter the collapsed index grouping", {
  test <- collapse_index(test_time$date, "2 day")
  expect_equal(test,
               as.Date(c("2017-12-02", "2017-12-02", "2017-12-03")))
})
