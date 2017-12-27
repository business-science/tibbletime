context("reconstruct testing")

# Test objects

test_time <- tibble::tibble(
  date   = c(as.Date("2017-12-01"), as.Date("2017-12-02"), as.Date("2017-12-03")),
  value  = c(1, 2, 3),
  group1 = c("a", "a", "b"),
  group2 = c("d", "e", "e")
)

test_tbl_time <- as_tbl_time(test_time, date)

# Tests

test_that("Remove index then reconstruct results in tibble", {

  no_index <- select(test_tbl_time, -date)
  no_index <- reconstruct(no_index, test_tbl_time)

  expect_equal(length(class(no_index)), 3)
})

test_that("Not removing index then reconstruct results in tbl_time", {

  with_index <- select(test_tbl_time, date)
  with_index <- reconstruct(with_index, test_tbl_time)

  expect_is(with_index, "tbl_time")
})
