context("new testing")

# Test objects

data(FB)
data(FANG)

# Tests

test_that("new_tbl_time() creates valid tbl_time objects", {

  FB_time <- new_tbl_time(FB, rlang::quo(date), "UTC")

  expect_is(FB_time, "tbl_time")
  expect_equal(get_index_time_zone(FB_time), "UTC")
  expect_equal(get_index_char(FB_time), "date")
  expect_equal(get_index_quo(FB_time), rlang::quo(date))
})

test_that("subclasses of tbl_time can be created", {

  FB_time <- new_tbl_time(FB, rlang::quo(date), "UTC", subclass = "sub_tbl_time")

  expect_is(FB_time, "sub_tbl_time")
  expect_is(FB_time, "tbl_time")
})

test_that("new_grouped_tbl_time() creates valid grouped_tbl_time objects", {
  
  FB_time <- new_grouped_tbl_time(FANG %>% dplyr::group_by(symbol), rlang::quo(date), "UTC")
  
  expect_is(FB_time, "grouped_tbl_time")
  expect_is(FB_time, "tbl_time")
  expect_equal(get_index_time_zone(FB_time), "UTC")
  expect_equal(get_index_char(FB_time), "date")
  expect_equal(get_index_quo(FB_time), rlang::quo(date))
})
