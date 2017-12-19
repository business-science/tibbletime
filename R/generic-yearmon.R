
#### General -------------------------------------------------------------------


#### create_series -------------------------------------------------------------

lookup_seq_fun.yearmon <- function(x) {
  seq.yearmon
}

#### parse_period --------------------------------------------------------------

assert_period_matches_index_class.yearmon <- function(x, period) {
  assertthat::assert_that(
    period %in% c("year", "quarter", "month"),
    msg = "Only year, quarter, and month periods are allowed for an index of class yearmon"
  )
}

#### parse_time_formula --------------------------------------------------------

lookup_defaults.yearmon <- function(index, side = "lhs") {
  switch(side,
         "lhs" = list(y = 1970, m = 01),
         "rhs" = list(y = 1970, m = 12))
}

list_to_datetime.yearmon <- function(index, tf_side, ...) {
  tf_side$d <- 1
  zoo::as.yearmon(list_to_datetime.Date(index, tf_side))
}

#### time_collapse -------------------------------------------------------------

dispatch_max_collapse.yearmon <- function(dummy, x) {
  index_char      <- get_index_char(x)
  index_sym       <- rlang::sym(index_char)
  index_time_zone <- get_index_time_zone(.data)
  index_class     <- get_index_class(x)

  # yearmon doesn't work well with vectorized calcs
  # Instead, max the numeric version and convert back
  x <- dplyr::mutate(
    x,
    !! index_sym := to_posixct_numeric(!! index_sym) %>% max()
  )

  # Convert back to yearmon after mutate call
  x[[index_char]] <- posixct_numeric_to_datetime(
    x[[index_char]],
    class = index_class,
    tz = index_time_zone
  )

  x
}

#### time_group ----------------------------------------------------------------

coerce_start_date.yearmon <- function(x, start_date) {
  zoo::as.yearmon(start_date)
}
