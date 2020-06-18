test_that("ungroup() works", {
  df <- tibble::tibble(
    group = c("g1", "g1", "g2"),
    date  = as.Date(c("2017-12-01", "2017-12-02", "2017-12-03"))
  )

  df <- as_tbl_time(df, date)
  df <- dplyr::group_by(df, group)

  expect_s3_class(
    dplyr::ungroup(df),
    c("tbl_time", "tbl_df", "tbl", "data.frame"),
    exact = TRUE
  )
})



