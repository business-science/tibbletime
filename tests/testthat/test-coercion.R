test_that("coercing tbl_time to tibble works", {
  df <- as_tbl_time(FANG, date)
  x <- as_tibble(df)

  expect_s3_class(x, c("tbl_df", "tbl", "data.frame"), exact = TRUE)

  # Ensure attributes are dropped
  expect_null(attr(x, "index_quo"))
  expect_null(attr(x, "index_time_zone"))
})

test_that("coercing grouped_tbl_time to tibble drops groupedness", {
  df <- as_tbl_time(FANG, date)
  gdf <- group_by(df, symbol)
  x <- as_tibble(gdf)

  expect_s3_class(x, c("tbl_df", "tbl", "data.frame"), exact = TRUE)
})
