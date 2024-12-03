#### TO POSIXct NUMERIC

to_posixct_numeric <- function(index) {
  UseMethod("to_posixct_numeric")
}

#' @export
to_posixct_numeric.default <- function(index) {
  as.numeric(index)
}

#' @export
to_posixct_numeric.Date <- function(index) {
  secs_in_day <- 86400
  as.numeric(.POSIXct(unclass(index) * secs_in_day, tz = get_default_time_zone()))
}

#' @export
to_posixct_numeric.POSIXct <- function(index) {
  as.numeric(index)
}

#' @export
to_posixct_numeric.yearmon <- function(index) {
  to_posixct_numeric(
    yearmon_to_POSIXct(index)
  )
}

# Same as yearmon, represented as a numeric internally, same as yearmon
#' @export
to_posixct_numeric.yearqtr <- to_posixct_numeric.yearmon

#' @export
to_posixct_numeric.hms <- function(index) {
  # No need to convert to POSIXct then numeric, this is just number of
  # seconds since epoch
  as.numeric(index)
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

#' @export
dispatch_to_datetime.default <- function(dummy, x, ..., tz = NULL) {
  tz <- tz %||% get_default_time_zone()
  as.POSIXct(x, tz = tz, origin = "1970-01-01", ...)
}

#' @export
dispatch_to_datetime.Date <- function(dummy, x, ..., tz = NULL) {
  tz <- tz %||% get_default_time_zone()
  as.Date(dispatch_to_datetime.default(dummy, x, tz = tz), tz = tz)
}

#' @export
dispatch_to_datetime.yearmon <- function(dummy, x, ..., tz = NULL) {
  zoo::as.yearmon(dispatch_to_datetime.default(dummy, x, tz = tz))
}

#' @export
dispatch_to_datetime.yearqtr <- function(dummy, x, ..., tz = NULL) {
  zoo::as.yearqtr(dispatch_to_datetime.default(dummy, x, tz = tz))
}

#' @export
dispatch_to_datetime.hms <- function(dummy, x, ..., tz = NULL) {
  datetime <- dispatch_to_datetime.default(dummy, x, tz = tz)
  hms::as_hms(datetime)
}
