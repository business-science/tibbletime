
#### General -------------------------------------------------------------------

# A date object will be in the correct form for making a time_formula
validate_side.Date <- function(x) {
  x
}

#### create_series -------------------------------------------------------------

lookup_seq_fun.Date <- function(x) {
  seq.Date
}

#### parse_period --------------------------------------------------------------

assert_period_matches_index_class.Date <- function(x, period) {
  assertthat::assert_that(
    period %in% c("year", "quarter", "month", "week", "day"),
    msg = "Only year, quarter, month, week, and day periods are allowed for an index of class Date"
  )
}

#### parse_time_formula --------------------------------------------------------

lookup_defaults.Date <- function(index, side = "lhs") {
  switch(side,
         "lhs" = list(y = 1970, m = 01, d = 01),
         "rhs" = list(y = 1970, m = 12, d = 00))
}

list_to_datetime.Date <- function(index, tf_side, ...) {
  lubridate::make_date(tf_side$y, tf_side$m, tf_side$d)
}

#### time_collapse -------------------------------------------------------------


#### time_group ----------------------------------------------------------------

coerce_start_date.Date <- function(x, start_date) {
  as.Date(start_date)
}
