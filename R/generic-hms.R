
#### General -------------------------------------------------------------------


#### create_series -------------------------------------------------------------

lookup_seq_fun.hms <- function(x) {
  seq.hms
}

push_datetime.hms <- function(x, push) {
  hms::as.hms(push_datetime.default(x, push))
}

#### parse_period --------------------------------------------------------------

assert_period_matches_index_class.hms <- function(x, period) {
  assertthat::assert_that(
    period %in% c("hour", "min", "sec"),
    msg = "Only hour, minute and second periods are allowed for an index of class hms"
  )
}

#### parse_time_formula --------------------------------------------------------

lookup_defaults.hms <- function(index, side = "lhs") {
  switch(side,
         "lhs" = list(h = 00, M = 00, s = 00),
         "rhs" = list(h = 23, M = 59, s = 59))
}

list_to_datetime.hms <- function(index, tf_side, ...) {
  hms::hms(seconds = tf_side$s, minutes = tf_side$M, hours = tf_side$h)
}

#### time_collapse -------------------------------------------------------------


#### time_group ----------------------------------------------------------------

coerce_start_date.hms <- function(x, start_date) {
  hms::as.hms(start_date)
}
