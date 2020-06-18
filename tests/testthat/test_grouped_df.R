context("grouped_df testing")

# Test objects

test_time <- tibble::tibble(
  group = c("g1", "g1", "g2"),
  date  = c(as.Date("2017-12-01"), as.Date("2017-12-02"), as.Date("2017-12-03")),
  value  = c(1, 2, 3),
  value2 = c(2, 5, 6),
  value3 = c(1, 3, 7)
)

test_grouped_tbl_time <- as_tbl_time(test_time, date) %>%
  group_by(group)

# Tests

test_that("Ungroup works", {
  test_ungrouped_tbl_time <- test_grouped_tbl_time %>%
    ungroup()

  test_ungrouped_tbl_time %>% class()

  expect_true(!"grouped_tbl_time" %in% class(test_ungrouped_tbl_time))

})



