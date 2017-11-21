context("tidyr compatability")

# Test objects

data(FANG)

FANG_g <- FANG %>%
  dplyr::group_by(symbol) %>%
  dplyr::slice(1:10)

FANG_g_time <- FANG_g %>%
  as_tbl_time(date)

FANG_time <- FANG %>%
  as_tbl_time(date) %>%
  dplyr::slice(1:10)

# Tests

test_that("nest() with index creates tbl_df", {

  FANG_nested <- FANG_g_time %>% tidyr::nest()

  expect_is(FANG_nested, "tbl_df")
  expect_is(FANG_nested$data[[1]], "tbl_time")
})

test_that("nest() without index stays tbl_time", {

  # Can't use grouped_df with -date, tidyr::nest only chooses groups
  FANG_nested <- FANG_time %>% tidyr::nest(-date)

  expect_is(FANG_nested, "tbl_time")
})

test_that("unnest() with index goes back to tbl_time", {

  FANG_unnested <- FANG_g_time %>% tidyr::nest() %>% tidyr::unnest()

  expect_is(FANG_unnested, "tbl_time")
  expect_equal(get_index_col(FANG_g_time), get_index_col(FANG_unnested))
})

test_that("unnest() without index stays tbl_time", {

  FANG_unnested <- FANG_time %>% tidyr::nest(-symbol, -date) %>% tidyr::unnest()

  expect_is(FANG_unnested, "tbl_time")
  expect_equal(get_index_col(FANG_time), get_index_col(FANG_unnested))
})
