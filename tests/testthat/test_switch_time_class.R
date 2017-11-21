context("switch_time_class testing")

# Test objects

test_time <- tibble::tibble(date = as.POSIXct("2017-12-01 00:12:15", tz = "UTC")) %>%
  as_tbl_time(date)

# Tests

test_that("Can change class correctly", {
  date_char    <- "2017-12-01 00:12:15"
  date_Date    <- as.Date(date_char)
  date_yearmon <- zoo::as.yearmon(date_char)
  date_yearqtr <- zoo::as.yearqtr(date_char)
  date_hms     <- hms::as.hms(as.POSIXct(date_char, tz = "UTC"))
  
  test_date_vec <- test_time$date
  
  expect_equal(switch_time_class(test_date_vec, "Date"),
               as.Date(date_char))
  
  expect_equal(switch_time_class(test_date_vec, "yearmon"),
               zoo::as.yearmon(date_char))
  
  expect_equal(switch_time_class(test_date_vec, "yearqtr"),
               zoo::as.yearqtr(as.Date(date_char)))
  
  expect_equal(switch_time_class(test_date_vec, "hms"),
               hms::as.hms(as.POSIXct(date_char, tz = "UTC")))
  
})