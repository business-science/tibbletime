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

test_that("start_date throws a warning of deprecation", {
  expect_warning(collapse_index(test_time$date, "2 day", start_date = "2017-11-30"))
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
