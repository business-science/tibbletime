
#### General -------------------------------------------------------------------

#### create_series -------------------------------------------------------------

lookup_seq_fun.yearqtr <- function(x) {
  seq.yearqtr
}

#### parse_period --------------------------------------------------------------

assert_period_matches_index_class.yearqtr <- function(x, period) {
  assertthat::assert_that(
    period %in% c("year", "quarter"),
    msg = "Only year and quarter periods are allowed for an index of class yearqtr"
  )
}

#### parse_time_formula --------------------------------------------------------

lookup_defaults.yearqtr <- function(index, side = "lhs") {
  switch(side,
         "lhs" = list(y = 1970, q = 01),
         "rhs" = list(y = 1970, q = 04))
}

list_to_datetime.yearqtr <- function(index, tf_side, ...) {
  yearqtr_string <- paste0(tf_side$y, "-", tf_side$q)
  zoo::as.yearqtr(yearqtr_string)
}

#### time_collapse -------------------------------------------------------------

# Same problem as yearmon, code is generic enough that this works
dispatch_max_collapse.yearqtr <- dispatch_max_collapse.yearmon

#### time_group ----------------------------------------------------------------

coerce_start_date.yearqtr <- function(x, start_date) {
  zoo::as.yearqtr(start_date)
}
