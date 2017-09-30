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
                          include_end = FALSE, tz = NULL, force_class = NULL,
                          as_date_vector = FALSE) {

  period_list <- split_period(period)

  # Switch based on class of the period
  if(period_list[["period"]] %in% c("day", "month", "year", "quarter", "week")) {
    date_fun <- as.Date
    seq_fun  <- seq.Date

    # Override if force_class to POSIXct
    if(!is.null(force_class)) {
      if(force_class == "POSIXct") {
        date_fun <- as.POSIXct
        seq_fun  <- seq.POSIXt
      }
    }

  } else {
    date_fun <- as.POSIXct
    seq_fun  <- seq.POSIXt
  }

  # If time zone is missing, default
  if(is.null(tz)) {
    tz <- Sys.timezone()
  }

  # Period to character
  time_char <- formula_to_char(time_formula)

  # Normalize
  from <- normalize_date(time_char[1], "from")
  to   <- normalize_date(time_char[2], "to")

  # Validate time_formula date order
  validate_date_order(from, to)

  # Coerce to dates
  from <- date_fun(from, tz = tz)
  to   <- date_fun(to,   tz = tz)

  # Create the sequence
  date_seq <- seq_fun(from, to, by = paste(period_list[["num"]], period_list[["period"]]))

  # Add the end date if required
  if(include_end) {
    if(max(date_seq) < to) {
      date_seq <- unique(c(date_seq, to))
    }
  }

  # Convert to tbl_time
  if(as_date_vector) {
    date_seq
  } else {
    as_tbl_time(tibble::tibble(date = date_seq), date)
  }
}
