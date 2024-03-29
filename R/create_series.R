#' Create a `tbl_time` object with a sequence of regularly spaced dates
#'
#' [create_series()] allows the user to quickly create a `tbl_time` object with
#' a `date` column populated with a sequence of dates.
#'
#' @inheritParams partition_index
#' @param time_formula A period to create the series over.
#' This is specified as a formula.
#' See the `Details` section of [filter_time()] for more information.
#' @param include_end Whether to always include the RHS of the `time_formula`
#' even if it does not match the regularly spaced index.
#' @param tz Time zone of the new series.
#' @param class One of `"Date"`, `"POSIXct"`, `"hms"`, `"yearmon"`, `"yearqtr"`.
#' The default is `"POSIXct"`.
#' @param as_vector Should the series be returned as a vector instead of
#' a tibble?
#'
#' @examples
#'
#' # Every day in 2013
#' create_series(~'2013', 'day')
#'
#' # Every other day in 2013
#' create_series(~'2013', '2 d')
#'
#' # Every quarter in 2013
#' create_series(~'2013', '1 q')
#'
#' # Daily series for 2013-2015
#' create_series('2013' ~ '2015', '1 d')
#'
#' # Minute series for 2 months
#' create_series('2012-01' ~ '2012-02', 'M')
#'
#' # Second series for 2 minutes
#' create_series('2011-01-01 12:10:00' ~ '2011-01-01 12:12:00', 's')
#'
#' # Date class
#' create_series(~'2013', 'day', class = "Date")
#'
#' # yearmon class
#' create_series(~'2013', 'month', class = "yearmon")
#'
#' # hms class. time_formula specified as HH:MM:SS here
#' create_series('00:00:00' ~ '12:00:00', 'second' , class = "hms")
#'
#' # Subsecond series
#' create_series('2013' ~ '2013-01-01 00:00:01', period = "10 millisec")
#' milli <- create_series('2013' ~ '2013-01-01 00:00:01', period = ".1 sec")
#' # Check that 'milli' is correct by running:
#' # options("digits.secs" = 4)
#' # options("digits" = 18)
#' # milli$date
#' # as.numeric(milli$date)
#'
#'
#' @export
create_series <- function(time_formula, period = "day",
                          class = "POSIXct", include_end = FALSE,
                          tz = "UTC", as_vector = FALSE) {

  period_list <- parse_period(period)

  # Generic validation based on the class
  dummy_index <- make_dummy_dispatch_obj(class)
  assert_allowed_datetime(dummy_index)
  assert_period_matches_index_class(dummy_index, period_list$period)

  # Get seq_* functions
  seq_fun <- lookup_seq_fun(dummy_index)

  # Parse the time_formula, don't convert to dates yet
  tf_list <- parse_time_formula(dummy_index, time_formula)

  #### Could allow for multifilter idea here, but instead applied to series

  # Then convert to datetime
  from_to <- purrr::map(tf_list, ~list_to_datetime(dummy_index, .x, tz = tz))

  # Get sequence creation pieces ready
  from <- from_to[[1]]
  to   <- from_to[[2]]
  by   <- create_by(period_list)

  # Final assertion of order
  assert_from_before_to(from, to)

  # Create the sequence
  date_seq <- seq_fun(from, to, by = by)

  # Add the end date if required
  if(include_end) {
    if(max(date_seq) < to) {
      date_seq <- push_datetime(date_seq, to)
    }
  }

  # Convert to tbl_time
  if(as_vector) {
    date_seq
  } else {
    as_tbl_time(tibble::tibble(date = date_seq), date)
  }

}

#### Utils ---------------------------------------------------------------------

assert_allowed_datetime <- function(x) {
  assertthat::assert_that(
    inherits_allowed_datetime(x),
    msg = glue::glue("Specified class, '{class(x)}', ",
                     "is not one of the allowed time-based classes")
  )
}

assert_from_before_to <- function(from, to) {
  from_num <- to_posixct_numeric(from)
  to_num   <- to_posixct_numeric(to)

  assertthat::assert_that(
    from_num <= to_num,
    msg = glue::glue("Incorrect expanded time_formula. ",
                     "`from`, {from}, must be before `to`, {to}")
  )
}

create_by <- function(period_list) {
  by <- paste(period_list$freq, period_list$period)
  by <- check_fractional_seconds(by, period_list)
  by
}

check_fractional_seconds <- function(by, period_list) {
  # For fractional seconds, the `by` argument must be numeric, not character
  if(period_list$freq < 1 && period_list$period == "sec") {
    by <- period_list$freq
  }
  by
}
