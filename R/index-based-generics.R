# Generic functions that define the differences between date/time classes

# Checks if the input is a supported date/time class
inherits_allowed_datetime <- function(x) {
  inherits(x, c("Date", "POSIXct", "POSIXt", "yearmon", "yearqtr", "hms"))
}

#### General -------------------------------------------------------------------


#### create_series -------------------------------------------------------------

## -------------------------------------------------------------------------
## lookup_seq_fun()

# For sequence creation in create_series()
lookup_seq_fun <- function(x) {
  UseMethod("lookup_seq_fun")
}

lookup_seq_fun.POSIXct <- function(x) {
  seq.POSIXt
}

lookup_seq_fun.Date <- function(x) {
  seq.Date
}

lookup_seq_fun.yearmon <- function(x) {
  seq.yearmon
}

lookup_seq_fun.yearqtr <- function(x) {
  seq.yearqtr
}

lookup_seq_fun.hms <- function(x) {
  seq.hms
}

## -------------------------------------------------------------------------
## push_datetime()

# Really only necessary because c(hms, hms) loses the hms class
push_datetime <- function(x, push) {
  UseMethod("push_datetime")
}

push_datetime.default <- function(x, push) {

  x_num    <- to_posixct_numeric(x)
  push_num <- to_posixct_numeric(push)

  pushed <- unique(c(x_num, push_num))

  posixct_numeric_to_datetime(
    pushed,
    class = get_index_col_class(x_num),
    tz = get_index_col_time_zone(x_num)
  )
}

push_datetime.hms <- function(x, push) {
  hms::as.hms(push_datetime.default(x, push))
}


#### parse_period --------------------------------------------------------------

## -------------------------------------------------------------------------
## assert_period_matches_index_class()

# Check that the supplied period formula is allowed for that class
assert_period_matches_index_class <- function(x, period) {
  UseMethod("assert_period_matches_index_class")
}

assert_period_matches_index_class.default <- function(x, period) {
  glue_stop("Class '{class(x)}' is not a known index class.")
}

assert_period_matches_index_class.POSIXct <- function(x, period) {
  return()
}

assert_period_matches_index_class.Date <- function(x, period) {
  assertthat::assert_that(
    period %in% c("year", "quarter", "month", "week", "day"),
    msg = "Only year, quarter, month, week, and day periods are allowed for an index of class Date"
  )
}

assert_period_matches_index_class.yearmon <- function(x, period) {
  assertthat::assert_that(
    period %in% c("year", "quarter", "month"),
    msg = "Only year, quarter, and month periods are allowed for an index of class yearmon"
  )
}

assert_period_matches_index_class.yearqtr <- function(x, period) {
  assertthat::assert_that(
    period %in% c("year", "quarter"),
    msg = "Only year and quarter periods are allowed for an index of class yearqtr"
  )
}

assert_period_matches_index_class.hms <- function(x, period) {
  assertthat::assert_that(
    period %in% c("hour", "min", "sec"),
    msg = "Only hour, minute and second periods are allowed for an index of class hms"
  )
}


#### parse_time_formula --------------------------------------------------------

## -------------------------------------------------------------------------
## split_to_list()

split_to_list <- function(x) {
  UseMethod("split_to_list")
}

#' @export
split_to_list.default <- function(x) {
  stop("Unrecognized time formula input")
}

#' @export
split_to_list.Date <- function(x) {
  x_lt <- as.POSIXlt(x, tz = get_default_time_zone())
  list(x_lt$year + 1900, x_lt$mon + 1, x_lt$mday)
}

#' @export
split_to_list.POSIXct <- function(x) {
  x_lt <- as.POSIXlt(x, tz = attr(x, "tzone"))
  list(x_lt$year + 1900, x_lt$mon + 1, x_lt$mday,
       x_lt$hour,        x_lt$min, x_lt$sec)
}

#' @export
split_to_list.yearmon <- function(x) {
  x_lt <- as.POSIXlt(x, tz = get_default_time_zone())
  list(x_lt$year + 1900, x_lt$mon + 1)
}
#' @export
split_to_list.yearqtr <- function(x) {
  x_lt <- as.POSIXlt(x, tz = get_default_time_zone())
  list(x_lt$year + 1900, x_lt$mon + 1)
}

#' @export
split_to_list.hms <- function(x) {
  x_lt <- as.POSIXlt(x, tz = get_default_time_zone())
  list(x_lt$hour, x_lt$min, x_lt$sec)
}

#' @export
split_to_list.character <- function(x) {
  # Split on - / , : * + space (notably not .)
  split_str <- unlist(strsplit(x, "-|/|:|[*]|[+]|[,]|[[:space:]]"))

  # Remove the "" that get left
  split_str <- split_str[split_str != ""]

  split_list <- as.list(split_str)

  maybe_to_numeric <- function(x) {
    if(x != ".") {
      x <- suppressWarnings(as.numeric(x))
      if(is.na(x)) {
        stop("Cannot parse time formula specification", call. = FALSE)
      }
    }
    x
  }

  # Attempt to coerce to numeric unless '.'
  split_list <- lapply(
    split_list,
    maybe_to_numeric
  )

  split_list
}

## -------------------------------------------------------------------------
## lookup_defaults()

# Find the default time_formula list values. These get overwritten
# with the user supplied values
lookup_defaults <- function(index, side = "lhs") {
  UseMethod("lookup_defaults")
}

lookup_defaults.POSIXct <- function(index, side = "lhs") {
  switch(side,
         "lhs" = list(y = 1970, m = 01, d = 01, h = 00, M = 00, s = 00),
         "rhs" = list(y = 1970, m = 12, d = 00, h = 23, M = 59, s = 59))
}

lookup_defaults.Date <- function(index, side = "lhs") {
  switch(side,
         "lhs" = list(y = 1970, m = 01, d = 01),
         "rhs" = list(y = 1970, m = 12, d = 00))
}

lookup_defaults.yearmon <- function(index, side = "lhs") {
  switch(side,
         "lhs" = list(y = 1970, m = 01),
         "rhs" = list(y = 1970, m = 12))
}

lookup_defaults.yearqtr <- function(index, side = "lhs") {
  switch(side,
         "lhs" = list(y = 1970, q = 01),
         "rhs" = list(y = 1970, q = 04))
}

lookup_defaults.hms <- function(index, side = "lhs") {
  switch(side,
         "lhs" = list(h = 00, M = 00, s = 00),
         "rhs" = list(h = 23, M = 59, s = 59))
}

## -------------------------------------------------------------------------
## list_to_datetime()

# Collapse the list of period values into a real datetime class
list_to_datetime <- function(index, tf_side, ...) {
  UseMethod("list_to_datetime")
}

list_to_datetime.POSIXct <- function(index, tf_side, tz, ...) {
  lubridate::make_datetime(tf_side$y, tf_side$m, tf_side$d,
                           tf_side$h, tf_side$M, tf_side$s, tz = tz)
}

list_to_datetime.Date <- function(index, tf_side, ...) {
  lubridate::make_date(tf_side$y, tf_side$m, tf_side$d)
}

list_to_datetime.yearmon <- function(index, tf_side, ...) {
  tf_side$d <- 1
  zoo::as.yearmon(list_to_datetime.Date(index, tf_side))
}

list_to_datetime.yearqtr <- function(index, tf_side, ...) {
  yearqtr_string <- paste0(tf_side$y, "-", tf_side$q)
  zoo::as.yearqtr(yearqtr_string)
}

list_to_datetime.hms <- function(index, tf_side, ...) {
  hms::hms(seconds = tf_side$s, minutes = tf_side$M, hours = tf_side$h)
}

#### partition_index -----------------------------------------------------------

## -------------------------------------------------------------------------
## coerce_start_date()

# Coerce a character start_date to a real datetime
coerce_start_date <- function(x, start_date) {
  UseMethod("coerce_start_date")
}

coerce_start_date.POSIXct <- function(x, start_date) {
  tz <- get_index_col_time_zone(x)
  as.POSIXct(start_date, tz = tz)
}

coerce_start_date.Date <- function(x, start_date) {
  as.Date(start_date)
}

coerce_start_date.yearmon <- function(x, start_date) {
  zoo::as.yearmon(start_date)
}

coerce_start_date.yearqtr <- function(x, start_date) {
  zoo::as.yearqtr(start_date)
}

coerce_start_date.hms <- function(x, start_date) {
  hms::as.hms(start_date, tz = get_index_col_time_zone(start_date))
}
