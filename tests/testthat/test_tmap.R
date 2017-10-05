context("tmap testing")

# Test objects

data(FB)
test_time <- FB %>% dplyr::select(-symbol)
test_tbl_time <- as_tbl_time(test_time, date)

data(FANG)
test_tbl_time_g <- as_tbl_time(FANG, date) %>%
  dplyr::group_by(symbol)

# Tests

test_that("tmap returns a list column", {
  test_tmap <- tmap(test_tbl_time, mean, "yearly")
  expect_is(test_tmap$data[[1]], "list")
})

test_that("tmap_dbl returns a numeric", {
  test_tmap <- tmap_dbl(test_tbl_time, mean, "yearly")
  expect_is(test_tmap$data[[1]], "numeric")
})

test_that("tmap_int returns a integer", {
  test_tmap <- tmap_int(test_tbl_time, length, "yearly")
  expect_is(test_tmap$data[[1]], "integer")
})

test_that("tmap_lgl returns a logical", {
  test_tmap <- tmap_lgl(test_tbl_time, is.numeric, "yearly")
  expect_is(test_tmap$data[[1]], "logical")
})

test_that("tmap_chr returns a character", {
  test_tmap <- tmap_chr(test_tbl_time, ~as.character(mean(.x)), "yearly")
  expect_is(test_tmap$data[[1]], "character")
})

test_that("tmap_dfc returns a tibble", {
  test_tmap <- tmap_dfc(test_tbl_time, mean, "yearly")
  expect_is(test_tmap$data[[1]], "tbl_df")
})

test_that("tmap_dfr returns a tibble", {
  test_tmap <- tmap_dfr(test_tbl_time, mean, "yearly")
  expect_is(test_tmap$data[[1]], "tbl_df")
})

test_that("tmap_df returns a tibble", {
  test_tmap <- tmap_df(test_tbl_time, mean, "yearly")
  expect_is(test_tmap$data[[1]], "tbl_df")
})

test_that("Error with non tbl_time object", {
  expect_error(tmap(test_time, mean, "yearly"),
               "Object is not of class `tbl_time`.")
  expect_error(tmap_chr(test_time, mean, "yearly"),
               "Object is not of class `tbl_time`.")
  expect_error(tmap_dbl(test_time, mean, "yearly"),
               "Object is not of class `tbl_time`.")
  expect_error(tmap_lgl(test_time, mean, "yearly"),
               "Object is not of class `tbl_time`.")
  expect_error(tmap_int(test_time, mean, "yearly"),
               "Object is not of class `tbl_time`.")
  expect_error(tmap_dfc(test_time, mean, "yearly"),
               "Object is not of class `tbl_time`.")
  expect_error(tmap_dfr(test_time, mean, "yearly"),
               "Object is not of class `tbl_time`.")
  expect_error(tmap_df(test_time, mean, "yearly"),
               "Object is not of class `tbl_time`.")
})

test_that("Groups are respected", {

  test_tmap <- tmap(test_tbl_time_g, mean, "yearly")
  expect_equal(nrow(test_tmap), 16L)
  expect_is(test_tmap, c("grouped_df"))

  test_tmap <- tmap_dbl(test_tbl_time_g, mean, "yearly")
  expect_equal(nrow(test_tmap), 16L)

  test_tmap <- tmap_chr(test_tbl_time_g, ~as.character(mean(.x)), "yearly")
  expect_equal(nrow(test_tmap), 16L)

  test_tmap <- tmap_lgl(test_tbl_time_g, is.numeric, "yearly")
  expect_equal(nrow(test_tmap), 16L)

  test_tmap <- tmap_int(test_tbl_time_g, length, "yearly")
  expect_equal(nrow(test_tmap), 16L)

  test_tmap <- tmap_dfc(test_tbl_time_g, mean, "yearly")
  expect_equal(nrow(test_tmap), 16L)

  test_tmap <- tmap_dfr(test_tbl_time_g, mean, "yearly")
  expect_equal(nrow(test_tmap), 16L)

  test_tmap <- tmap_df(test_tbl_time_g, mean, "yearly")
  expect_equal(nrow(test_tmap), 16L)
})
