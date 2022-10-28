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

  FANG_nested <- FANG_g_time %>% tidyr::nest(data = everything())

  expect_is(FANG_nested, "tbl_df")
  expect_is(FANG_nested$data[[1]], "tbl_time")
})

test_that("nest() without index stays tbl_time", {

  # Can't use grouped_df with -date, tidyr::nest only chooses groups
  FANG_nested <- FANG_time %>% tidyr::nest(data = -date)

  expect_is(FANG_nested, "tbl_time")
})

test_that("nest() with .key is deprecated but works", {
  expect_warning(
    FANG_nested <- FANG_time %>% tidyr::nest(-date, .key = "stuff")
  )

  expect_is(FANG_nested, "tbl_time")
  expect_is(FANG_nested$stuff[[1]], "tbl_df")
})

test_that("unnest() with index returns tbl_df", {

  FANG_unnested <- FANG_g_time %>%
    tidyr::nest(data = everything()) %>%
    tidyr::unnest(cols = data)

  expect_s3_class(FANG_unnested, "tbl_df")
  expect_true(!"tbl_time" %in% class(FANG_unnested))

  # This used to return a `tbl_time` because we added a special
  # `unnest.tbl_time()` method that intercepted the unnesting. But that
  # was a horrible idea.
  # expect_is(FANG_unnested, "tbl_time")
  # expect_equal(get_index_col(FANG_g_time), get_index_col(FANG_unnested))
})

test_that("unnest() without index stays tbl_time", {

  FANG_unnested <- FANG_time %>% tidyr::nest(data = c(-symbol, -date)) %>% tidyr::unnest(cols = data)

  expect_is(FANG_unnested, "tbl_time")
  expect_equal(get_index_col(FANG_time), get_index_col(FANG_unnested))
})

test_that("unnest() with `...` is deprecated but works", {
  FANG_nested <- FANG_g_time %>% tidyr::nest(data1 = open, data2 = high)

  expect_warning(
    FANG_unnested <- tidyr::unnest(FANG_nested, data1, data2)
  )

  expect_is(FANG_unnested, "tbl_time")
})

test_that("can still do a normal unnest()", {
  mtcars_unnested <- mtcars %>%
    tidyr::nest(data = c(mpg, cyl)) %>%
    tidyr::unnest(cols = data)

  expect_is(mtcars_unnested, "tbl_df")
  expect_equal(sort(colnames(mtcars_unnested)), sort(colnames(mtcars)))
})
