context("time_group testing")
# time_group(index, period = "yearly", start_date = NULL, ...)

# Test objects

data(FB)
test_index <-FB$date
test_index_tbl <-FB %>% dplyr::select(date)

data(FANG)
test_index_g <- FANG$date
test_index_tbl_g <- FANG %>% dplyr::select(date)

# Tests

# is this, btw normal behaviour for FANG? What if it is run on ungrouped data?
#test_that("early return triggered and seq_along is returned", {
#  test <- time_group(index=test_index_g, period="hour")
#  expect_equal(test,  seq_along(test_index_g))
#})

test_that("Weekly period counting works", {
  test <- time_group(index=test_index_g, period = "weekly")
  test_filter_yw <- test_index_tbl_g %>%
    dplyr::mutate(year_week=paste(lubridate::isoyear(date), lubridate::isoweek(date), sep="-"))
  test_filter <- test_filter_yw %>% dplyr::group_by(year_week) %>%
    dplyr::summarise(date=max(date)) %>% dplyr::arrange(date) %>%
    dplyr::mutate(group_num=1:n()) %>% dplyr::select(-date) %>%
      dplyr::right_join(test_filter_yw, by="year_week") %>% dplyr::pull(group_num)
  expect_equal(test, test_filter)
})
