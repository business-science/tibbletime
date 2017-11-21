context("coercion testing")

# Test objects

test_df <- data.frame(
  date   = c(as.Date("2017-12-01"), as.Date("2017-12-02"), as.Date("2017-12-03")),
  value  = c(1, 2, 3),
  group1 = c("a", "a", "b"),
  group2 = c("d", "e", "e")
)

test_time <- tibble::as.tibble(test_df)

# Tests

test_that("Can coerce tbl_df to tbl_time", {
  # Manually make tbl_time
  test_time2 <- test_time
  attr(test_time2, "index_quo") <- rlang::quo(date)
  attr(test_time2, "index_time_zone") <- "UTC"
  class(test_time2) <- c("tbl_time", class(test_time2))
  
  expect_equal(as_tbl_time(test_time, date), test_time2)
})

test_that("Can coerce data.frame to tbl_time using default method", {
  expect_equal(as_tbl_time(test_df, date), as_tbl_time(test_time, date))
})

test_that("Can coerce grouped_df to tbl_time", {
  # tbl_time first then group
  test_time_g <- as_tbl_time(test_time, date) %>%
    group_by(group1)
  
  # group then tbl_time
  expect_equal(test_time %>% group_by(group1) %>% as_tbl_time(date), test_time_g)
})

test_that("Can coerce tbl_time back to tbl_df", {
  test_time2 <- as_tbl_time(test_time, date)
  expect_equal(tibble::as_tibble(test_time2), test_time)
})
