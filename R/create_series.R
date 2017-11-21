#' Create a `tbl_time` object with a sequence of regularly spaced dates
#'
#' [create_series()] allows the user to quickly create a `tbl_time` object with
#' a `date` column populated with a sequence of dates.
#'
#' @inheritParams time_group
#' @param time_formula A period to create the series over.
#' This is specified as a formula.
#' See the `Details` section of [time_filter()] for more information.
#' @param include_end Whether to always include the RHS of the `time_formula`
#' even if it does not match the regularly spaced index.
#' @param tz Time zone of the new series.
#' @param force_class Either `"Date"` or `"POSIXct"`. The default is to infer
#' the simplest class required from the `period` specified, but this will
#' override that.
#' @param as_date_vector Should the series be returned as a vector instead of
#' a tibble?
#'
#' @examples
#'
#' # Every day in 2013
#' create_series(~2013, "daily")
#'
#' # Every other day in 2013
#' create_series(~2013, 2~d)
#'
#' # Every quarter in 2013
#' create_series(~2013, 1~q)
#'
#' # Daily series for 2 years
#' create_series(2013~2015, 1~d)
#'
#' # Minute series for 2 months
#' create_series(2012-01~2012-02, 1~M)
#'
#' # Second series for 2 minutes
#' create_series(2011-01-01 + 12:10:00 ~ 2011-01-01 + 12:12:00, 1~s)
#'
#' # Force POSIXct class
#' create_series(~2013, 1~d, force_class = "POSIXct")
#'
#' @export
create_series <- function(time_formula, period = "daily",
                          class = "POSIXct", include_end = FALSE,
                          tz = "UTC", as_vector = FALSE) {

  period_list <- parse_period(period)

  # Generic validation based on the class
  dummy_index <- make_dummy_dispatch_obj(class)
  assert_allowed_datetime(dummy_index)
  assert_period_matches_index_class(dummy_index, period_list$period)

  # Get seq.* functions
  seq_fun <- lookup_seq_fun(dummy_index)

  # Parse the time_formula, don't convert to dates yet
  tf_list <- parse_time_formula(dummy_index, time_formula)

  # Could allow for multifilter idea here, but instead applied to series

  # Then convert to datetime
  from_to <- purrr::map(tf_list, ~list_to_datetime(dummy_index, .x, tz = tz))

  # Get sequence creation pieces ready
  from <- from_to[[1]]
  to   <- from_to[[2]]
  by   <- paste(period_list$freq, period_list$period)

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
    is_allowed_datetime(x),
    msg = "Specified `class` is not one of the allowed time-based classes"
  )
}

assert_from_before_to <- function(from, to) {
  from <- to_posixct_numeric(from)
  to   <- to_posixct_numeric(to)

  assertthat::assert_that(
    from <= to,
    msg = "`from` must be before `to`"
  )
}


