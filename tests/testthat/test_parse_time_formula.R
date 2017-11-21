context("parse_time_formula testing")

obj_Date <- make_dummy_dispatch_obj("Date")
obj_POSIXct <- make_dummy_dispatch_obj("POSIXct")
obj_yearmon <- make_dummy_dispatch_obj("yearmon")
obj_yearqtr <- make_dummy_dispatch_obj("yearqtr")
obj_hms <- make_dummy_dispatch_obj("hms")

# Tests

test_that("Basic parsing", {
  expect_equal(parse_time_formula(obj_Date, ~2013), 
               list(list(y = 2013, m = 1, d = 1), 
                    list(y = 2013, m = 12, d = c(Dec = 31))))

  expect_equal(parse_time_formula(obj_POSIXct, ~2013), 
               list(list(y = 2013, m = 1, d = 1, h = 0, M = 0, s = 0), 
                    list(y = 2013, m = 12, d = c(Dec = 31), h = 23, M = 59, s = 59)))
  
  expect_equal(parse_time_formula(obj_yearmon, ~2013), 
               list(list(y = 2013, m = 1), 
                    list(y = 2013, m = 12)))
  
  expect_equal(parse_time_formula(obj_yearqtr, ~2013), 
               list(list(y = 2013, q = 1), 
                    list(y = 2013, q = 4)))
  
  expect_equal(parse_time_formula(obj_hms, ~1), 
               list(list(h = 1, M = 0, s = 0), 
                    list(h = 1, M = 59, s = 59)))
})


test_that("Errors are thrown with incorrect specification", {
  expect_error(parse_time_formula(obj_Date, ~2013-01-01 / 1), 
               "For a Date index, time_formula can only include y, m, d specifications.")
  
  expect_error(parse_time_formula(obj_yearmon, ~2013-01-01), 
               "For a yearmon index, time_formula can only include y, m specifications.")
  
  expect_error(parse_time_formula(obj_yearqtr, ~2013-01-01), 
               "For a yearqtr index, time_formula can only include y, q specifications.")
  
  expect_error(parse_time_formula(obj_hms, ~2013-01-01 / 1), 
               "For a hms index, time_formula can only include h, M, s specifications.")
})
