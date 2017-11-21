# Split a full period spec into its frequency and periodicity
parse_period <- function(period) {
  UseMethod("parse_period")
}

parse_period.default <- function(period) {
  stop("Unsupported period specification. Only formulas or characters are allowed", call. = FALSE)
}

parse_period.character <- function(period) {
  period_char <- parse_period_rhs(period)
  list(freq = 1, period = period_char)
}

parse_period.formula <- function(period) {

  # Check LHS
  period_freq <- rlang::f_lhs(period)
  assert_freq_is_numeric(period_freq)

  # Check RHS
  period_char <- as.character(rlang::f_rhs(period))
  period_char <- parse_period_rhs(period_char)

  list(freq = period_freq, period = period_char)
}

#### Utils ---------------------------------------------------------------------

# Check that the RHS of period is correct
parse_period_rhs <- function(period) {

  if(stringr::str_length(period) == 1) {
    p <- parse_letter_period(period)
  } else {
    p <- parse_word_period(period)
  }

  p
}

# >1 letter character parsing
parse_word_period <- function(period) {

  partial_detect <- function(period, detect_pattern) {
    stringr::str_detect(period, stringr::coll(detect_pattern, TRUE))
  }

  p <- dplyr::case_when(
    partial_detect(period, "year")    ~ "year",
    partial_detect(period, "quarter") ~ "quarter",
    partial_detect(period, "month")   ~ "month",
    partial_detect(period, "week")    ~ "week",
    partial_detect(period, "da")      ~ "day",
    partial_detect(period, "hour")    ~ "hour",
    partial_detect(period, "min")     ~ "min",
    partial_detect(period, "sec")     ~ "sec",
   TRUE                               ~ "NULL"
  )

  if(p == "NULL") {
    stop("Period specified incorrectly.", call. = FALSE)
  }

  p
}

# 1 letter parsing, case sensitive
parse_letter_period <- function(period) {
  switch (period,
          "y" = "year",    "Y" = "year",
          "q" = "quarter", "Q" = "quarter",
          "m" = "month",    # Case sensitive
          "w" = "week",    "W" = "week",
          "d" = "day",     "D" = "day",
          "h" = "hour",    "H" = "hour",
          "M" = "min",      # Case sensitive
          "s" = "sec",     "S" = "sec",
          stop("Period specified incorrectly.", call. = FALSE)
  )
}

# Check that the LHS `period` number is correct
assert_freq_is_numeric <- function(freq) {
  assertthat::assert_that(
    is.numeric(freq),
    msg = "LHS of `period` formula must be numeric."
  )
}
