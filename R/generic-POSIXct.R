
#### General -------------------------------------------------------------------


#### create_series -------------------------------------------------------------

lookup_seq_fun.POSIXct <- function(x) {
  seq.POSIXt
}

#### parse_period --------------------------------------------------------------

assert_period_matches_index_class.POSIXct <- function(x, period) {
  return()
}

#### parse_time_formula --------------------------------------------------------

lookup_defaults.POSIXct <- function(index, side = "lhs") {
  switch(side,
         "lhs" = list(y = 1970, m = 01, d = 01, h = 00, M = 00, s = 00),
         "rhs" = list(y = 1970, m = 12, d = 00, h = 23, M = 59, s = 59))
}

list_to_datetime.POSIXct <- function(index, tf_side, tz, ...) {
  lubridate::make_datetime(tf_side$y, tf_side$m, tf_side$d, tf_side$h, tf_side$M, tf_side$s, tz = tz)
}

#### time_collapse -------------------------------------------------------------


#### time_group ----------------------------------------------------------------

coerce_start_date.POSIXct <- function(x, start_date) {
  tz <- get_index_col_time_zone(x)
  as.POSIXct(start_date, tz = tz)
}
