context("tidyr unnest")

# Test objects

test_time <- tibble::tibble(
  date  = c(as.Date("2017-12-01"), as.Date("2017-12-02"), as.Date("2017-12-03")),
  value1 = c(1, 2, 2),
  value2 = c(2, 3, 4)
)

test_tbl_time <- as_tbl_time(test_time, date)
nested_test_tbl_time   <- nest(test_tbl_time, -date)
nested_test            <- nest(test_time, -date)


# Tests

test_that("unnest works as expected", {
  expect_equal(unnest(nested_test_tbl_time),
               unnest(nested_test))
})

test_that("tbl_time class is retained", {
  expect_is(unnest(nested_test_tbl_time), "tbl_time")
})
