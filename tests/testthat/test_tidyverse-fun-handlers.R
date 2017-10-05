context("tidyverse function handlers")

# Test objects

test_time <- tibble::tibble(
  group = c("g1", "g1", "g2"),
  date  = c(as.Date("2017-12-01"), as.Date("2017-12-02"), as.Date("2017-12-03")),
  value = c(1, 2, 3)
)

test_tbl_time <- as_tbl_time(test_time, date)
grouped_test  <- dplyr::group_by(test_tbl_time, group)

time_classes <- stringr::str_subset(class(test_tbl_time), "tbl_time")
time_attrs <- list(
  index     = attr(test_tbl_time, "index"),
  time_zone = attr(test_tbl_time, "time_zone")
)

time_classes_g <- stringr::str_subset(class(grouped_test), "tbl_time")
time_attrs_g <- list(
  index     = attr(grouped_test, "index"),
  time_zone = attr(grouped_test, "time_zone")
)

# Tests

test_that("tidyverse_execute executes a dplyr function correctly", {
  expect_equal(tidyverse_execute(test_tbl_time, mutate, x_new=2),
               mutate(test_time, x_new = 2))
})

test_that("detime strips time based classes/attributes", {
  no_tbl_time <- detime(test_tbl_time, time_classes, time_attrs)
  expect_false("tbl_time" %in% class(no_tbl_time))
  expect_false("index" %in% names(attributes(no_tbl_time)))
  expect_false("time_zone" %in% names(attributes(no_tbl_time)))
})

test_that("detime strips time based classes/attributes with groups", {
  no_tbl_time <- detime(grouped_test, time_classes_g, time_attrs_g)
  expect_false("tbl_time" %in% class(no_tbl_time))
  expect_false("grouped_tbl_time" %in% class(no_tbl_time))
  expect_false("index" %in% names(attributes(no_tbl_time)))
  expect_false("time_zone" %in% names(attributes(no_tbl_time)))
})

test_that("retime adds time based attributes/classes back", {
  no_tbl_time <- detime(test_tbl_time, time_classes, time_attrs)
  retime_tbl_time <- retime(no_tbl_time, time_classes, time_attrs)
  expect_is(retime_tbl_time, "tbl_time")
  expect_match(names(attributes(retime_tbl_time)), "index", all = FALSE)
  expect_match(names(attributes(retime_tbl_time)), "time_zone", all = FALSE)
})

test_that("retime adds time based attributes/classes back with groups", {
  no_tbl_time <- detime(grouped_test, time_classes_g, time_attrs_g)
  retime_tbl_time <- retime(no_tbl_time, time_classes_g, time_attrs_g)
  expect_is(retime_tbl_time, "tbl_time")
  expect_is(retime_tbl_time, "grouped_tbl_time")
  expect_match(names(attributes(retime_tbl_time)), "index", all = FALSE)
  expect_match(names(attributes(retime_tbl_time)), "time_zone", all = FALSE)
})

test_that("Error with non tbl_time object", {
  expect_error(tidyverse_execute(test_time, mutate, x_new=2),
               "Object is not of class `tbl_time`.")
})
