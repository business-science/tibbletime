context("ceiling_index testing")

# Test objects

data(FB)
test_time <- FB
test_tbl_time <- as_tbl_time(test_time, date)

test_tbl_time <- test_tbl_time %>%
  dplyr::mutate(
    date_posix   = to_posixct_numeric(date) %>% posixct_numeric_to_datetime(class = "POSIXct", tz = "UTC"),
    date_yearmon = to_posixct_numeric(date) %>% posixct_numeric_to_datetime(class = "yearmon", tz = "UTC"),
    date_yearqtr = to_posixct_numeric(date) %>% posixct_numeric_to_datetime(class = "yearqtr", tz = "UTC")
  ) %>%
  dplyr::select(dplyr::contains("date"))

# Tests

test_that("Ceiling all Date/Datetime to yearly results in the same answer", {
  test <- purrr::map_dfc(test_tbl_time, ~ceiling_index(.x, "yearly") %>% to_posixct_numeric)
  expect_equal(test$date, test$date_posix)
  expect_equal(test$date, test$date_yearmon)
  expect_equal(test$date, test$date_yearqtr)
})

test_that("Ceiling works with hms", {
  hms_test <- create_series('01'~'12', period = "hourly", class = "hms")
  expect_equal(
    ceiling_index(hms_test$date, "12 hour"),
    rep(43200, 12) %>% hms::as_hms()
  )
})
