context("rollify testing")

# Test objects

test_time <- tibble::tibble(
  group = c("g1", "g1", "g2"),
  date  = c(as.Date("2017-12-01"), as.Date("2017-12-02"), as.Date("2017-12-03")),
  value  = c(1, 2, 3),
  value2 = c(2, 5, 6),
  value3 = c(1, 3, 7)
)

test_tbl_time      <- as_tbl_time(test_time, date)

# Tests

test_that("Basic roller() call works", {
  expect_equal(roller(x = c(1,2,3), .f = mean, window = 2),
               c(NA, 1.5, 2.5))
})

test_that("rollify() creates a function", {
  expect_is(rollify(mean), "function")
})

test_that("rollify() with function call works", {
  test_roll <- rollify(mean, window = 2)
  expect_equal(mutate(test_tbl_time, test = test_roll(value)),
               mutate(test_tbl_time, test = c(NA, 1.5, 2.5)))
})

test_that("rollify() with ~ specification works", {
  test_roll <- rollify(~mean(.x), window = 2)
  expect_equal(mutate(test_tbl_time, test = test_roll(value)),
               mutate(test_tbl_time, test = c(NA, 1.5, 2.5)))
})

test_that("rollify() with two args works", {
  test_roll <- rollify(~cor(.x, .y), window = 3)
  expect_equal(mutate(test_tbl_time, test = test_roll(value, value2)),
               mutate(test_tbl_time, test = c(NA, NA, cor(value, value2))))
})

test_that("rollify() with explicit function works for >2 args", {
  test_roll <- rollify(function(x, y, z) {sum(x + y + z)}, window = 3)
  expect_equal(mutate(test_tbl_time, test = test_roll(value, value2, value3)),
               mutate(test_tbl_time, test = c(NA, NA, sum(value + value2 + value3))))
})

test_that("rollify() result works alone", {
  test_roll <- rollify(~mean(.x), window = 2)
  expect_equal(test_roll(c(1,3,4)), c(NA, 2.0, 3.5))
})
