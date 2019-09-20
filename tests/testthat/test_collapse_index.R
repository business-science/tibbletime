context("collapse_index testing")

# Test objects

test_time <- tibble::tibble(
  date   = c(as.Date("2017-12-01"), as.Date("2017-12-02"), as.Date("2017-12-03")),
  value  = c(1, 2, 3),
  group1 = c("a", "a", "b"),
  group2 = c("d", "e", "e")
)

# Tests

test_that("Yearly collapse returns correct dates", {
  test <- collapse_index(test_time$date, "yearly")
  expect_equal(unique(test),
               as.Date("2017-12-03"))
})

test_that("side = 'start' returns start of period", {
  test <- collapse_index(test_time$date, "yearly", side = "start")
  expect_equal(unique(test),
               as.Date("2017-12-01"))
})

test_that("Index vectors can be passed to the period argument", {
  custom_period <- create_series("2017-11-30" ~ "2017-12-03", "2 day", "Date", as_vector = TRUE)
  test <- collapse_index(test_time$date, custom_period)
  expect_equal(test,
               as.Date(c("2017-12-01", "2017-12-03", "2017-12-03")))
})

test_that("Collapsing works on yearmon", {
  ex <- create_series(~'2017', "monthly", "yearmon")

  expect_equal(collapse_index(ex$date, "yearly"),
               zoo::as.yearmon(rep(2017.917, 12)))
})

test_that("Collapsing works on yearqtr", {
  ex <- create_series(~'2017', "quarter", "yearqtr")

  expect_equal(collapse_index(ex$date, "yearly"),
               zoo::as.yearqtr(rep(2017.75, 4)))
})

test_that("Collapsing works on hms", {
  ex <- create_series(~'12:00', "second", "hms")

  expect_equal(collapse_index(ex$date, "minute", side = "start"),
               hms::hms(rep(43200, 60)))
})

test_that("day becomes DSTday for POSIXct to prevent DST boundary problems", {
  seq_fun <- lookup_seq_fun(x = make_dummy_dispatch_obj("POSIXct"))

  ret <- seq_fun(as.POSIXct("2016-03-12", tz = "America/New_York"),
                 as.POSIXct("2016-03-14", tz = "America/New_York"),
                 "1 day")

  test <- as.POSIXct(c("2016-03-12", "2016-03-13", "2016-03-14"),
                     tz = "America/New_York")

  expect_equal(ret, test)
})

test_that("can use `collapse_by()` when a column is named `start_date` (#81)", {
  x <- data.frame(
    start_date = as.Date("2017-12-01") + 0:2,
    value  = c(1, 2, 3)
  )

  x <- as_tbl_time(x, start_date)

  expect_equal(
    collapse_by(x),
    dplyr::mutate(x, start_date = collapse_index(start_date))
  )

  expect_equal(
    collapse_by(x, start_date = as.Date("2017-01-01"), side = "start", period = "2 days"),
    dplyr::mutate(x, start_date = collapse_index(start_date, "2 days", start_date = as.Date("2017-01-01"), side = "start"))
  )

  expect_equal(
    collapse_by(x, start_date = as.Date("2016-12-31"), side = "start", period = "2 days"),
    dplyr::mutate(x, start_date = collapse_index(start_date, "2 days", start_date = as.Date("2016-12-31"), side = "start"))
  )
})


