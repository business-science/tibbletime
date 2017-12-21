#### TO POSIXct NUMERIC

to_posixct_numeric <- function(x) {
  UseMethod("to_posixct_numeric")
}

to_posixct_numeric.default <- function(x) {
  as.numeric(x)
}

to_posixct_numeric.Date <- function(x) {
  secs_in_day <- 86400
  as.numeric(.POSIXct(unclass(x) * secs_in_day, tz = get_default_time_zone()))
}

to_posixct_numeric.POSIXct <- function(x) {
  as.numeric(x)
}

to_posixct_numeric.yearmon <- function(x) {
  to_posixct_numeric(
    yearmon_to_POSIXct(x)
  )
}

# Same as yearmon, represented as a numeric internally, same as yearmon
to_posixct_numeric.yearqtr <- to_posixct_numeric.yearmon

to_posixct_numeric.hms <- function(x) {
  # No need to convert to POSIXct then numeric, this is just number of
  # seconds since epoch
  as.numeric(x)
}


# This is much faster than using as.POSIXct.yearmon which calls
# as.POSIXct.Date, it converts a character to a Date, very slow!
yearmon_to_POSIXct <- function(x) {
  x <- unclass(x)
  if (all(is.na(x))) {
    return(as.Date(x))
  }
  year  <- floor(x + 0.001)
  month <- floor(12 * (x - year) + 1 + 0.5 + 0.001)

  lubridate::make_datetime(year, month, 1, tz = get_default_time_zone())
}


#### FROM POSIXct NUMERIC

#' Converting a posixct numeric time back to a classed datetime
#'
#' @param x A posixct numeric vector
#' @param class The class to convert to
#' @param ... Extra arguments passed on the the specific coercion function
#' @param tz The time zone to convert to. The default UTC is used if none is
#' supplied
posixct_numeric_to_datetime <- function(x, class = "POSIXct", ..., tz = NULL) {
  dispatch_obj <- make_dummy_dispatch_obj(class)
  dispatch_to_datetime(dispatch_obj, x, ..., tz = tz)
}

# This picks the datetime class to convert back to
dispatch_to_datetime <- function(dummy, x, ...) {
  UseMethod("dispatch_to_datetime")
}

dispatch_to_datetime.default <- function(dummy, x, ..., tz = NULL) {
  tz <- tz %||% get_default_time_zone()
  as.POSIXct(x, tz = tz, origin = "1970-01-01", ...)
}

dispatch_to_datetime.Date <- function(dummy, x, ..., tz = NULL) {
  tz <- tz %||% get_default_time_zone()
  as.Date(dispatch_to_datetime.default(dummy, x, tz = tz), tz = tz)
}

dispatch_to_datetime.yearmon <- function(dummy, x, ..., tz = NULL) {
  zoo::as.yearmon(dispatch_to_datetime.default(dummy, x, tz = tz))
}

dispatch_to_datetime.yearqtr <- function(dummy, x, ..., tz = NULL) {
  zoo::as.yearqtr(dispatch_to_datetime.default(dummy, x, tz = tz))
}

dispatch_to_datetime.hms <- function(dummy, x, ..., tz = NULL) {
  hms::as.hms(dispatch_to_datetime.default(dummy, x, tz = tz))
}
