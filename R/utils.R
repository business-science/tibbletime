# Reexports --------------------------------------------------------------------

#' @importFrom magrittr %>%
#' @export
#'
magrittr::`%>%`

#' @importFrom rlang :=
#'
NULL

# Global Util ------------------------------------------------------------------

# Convert a period to an expanded list of syms required to expand the index
period_to_syms <- function(period) {
  groups <- switch (period,
      "yearly"     = list("year"),
      "quarterly"  = list("year", "quarter"),
      "monthly"    = list("year", "month"),
      "weekly"     = list("year", "month", "week"),
      "daily"      = list("year", "month", "day"),
      "hourly"     = list("year", "month", "day", "hour"),
      "minute"     = list("year", "month", "day", "hour", "minute"),
      "second"     = list("year", "month", "day", "hour", "minute", "second"),
      stop("`period` is not one of: 'yearly', 'quarterly', 'monthly', 'weekly',
           'daily', 'hourly', 'minute', 'second'", call. = FALSE)
  )

  rlang::syms(groups)
}


# Period to formula parsing
parse_period_to_formula <- function(period) {

  if(stringr::str_length(period) == 1) {

    p <- letter_period_to_formula(period)

  } else {

    if(stringr::str_detect(period, stringr::coll("year", TRUE))) {
      p <- 1~y
    } else if(stringr::str_detect(period, stringr::coll("quarter", TRUE))) {
      p <- 1~q
    } else if(stringr::str_detect(period, stringr::coll("month", TRUE))) {
      p <- 1~m
    } else if(stringr::str_detect(period, stringr::coll("week", TRUE))) {
      p <- 1~w
    } else if(stringr::str_detect(period, stringr::coll("da", TRUE))) {
      p <- 1~d
    } else if(stringr::str_detect(period, stringr::coll("hour", TRUE))) {
      p <- 1~h
    } else if(stringr::str_detect(period, stringr::coll("min", TRUE))) {
      p <- 1~M
    } else if(stringr::str_detect(period, stringr::coll("sec", TRUE))) {
      p <- 1~s
    } else {
      stop("Character period specified incorrectly.", call. = FALSE)
    }

  }

  p
}


# Check that the RHS `by` period is correct
parse_period <- function(period) {

  if(stringr::str_length(period) == 1) {

    p <- letter_period(period)

  } else {

    if(stringr::str_detect(period, stringr::coll("year", TRUE))) {
      p <- "year"
    } else if(stringr::str_detect(period, stringr::coll("quarter", TRUE))) {
      p <- "quarter"
    } else if(stringr::str_detect(period, stringr::coll("month", TRUE))) {
      p <- "month"
    } else if(stringr::str_detect(period, stringr::coll("week", TRUE))) {
      p <- "week"
    } else if(stringr::str_detect(period, stringr::coll("da", TRUE))) {
      p <- "day"
    } else if(stringr::str_detect(period, stringr::coll("hour", TRUE))) {
      p <- "hour"
    } else if(stringr::str_detect(period, stringr::coll("min", TRUE))) {
      p <- "min"
    } else if(stringr::str_detect(period, stringr::coll("sec", TRUE))) {
      p <- "sec"
    } else {
      stop("RHS of `by` specified incorrectly.", call. = FALSE)
    }

  }

  p
}


# Check that the shorthand period format is correct
letter_period <- function(period) {
  switch (period,
          "y" = "year",
          "Y" = "year",
          "q" = "quarter",
          "Q" = "quarter",
          "m" = "month",   # Case sensitive
          "w" = "week",
          "W" = "week",
          "d" = "day",
          "D" = "day",
          "h" = "hour",
          "H" = "hour",
          "M" = "min",     # Case sensitive
          "s" = "sec",
          "S" = "sec",
          stop("RHS 1 letter period specified incorrectly.", call. = FALSE)
  )
}

# Check that the shorthand period format is correct
letter_period_to_formula <- function(period) {
  switch (period,
          "y" = 1~y,
          "Y" = 1~y,
          "q" = 1~q,
          "Q" = 1~q,
          "m" = 1~m,   # Case sensitive
          "w" = 1~w,
          "W" = 1~w,
          "d" = 1~d,
          "D" = 1~d,
          "h" = 1~h,
          "H" = 1~h,
          "M" = 1~M,     # Case sensitive
          "s" = 1~s,
          "S" = 1~s,
          stop("1 letter period specified incorrectly.", call. = FALSE)
  )
}


# Check that the LHS `period` number is correct
check_num <- function(num) {
  assertthat::assert_that(is.numeric(num),
                          msg = "LHS of `period` formula must be numeric")
  num
}


# Split `period` into parts
split_period <- function(period) {

  # Parse character period to formula
  if(is.character(period)) {
    period <- parse_period_to_formula(period)
  }

  # Check LHS
  period_num <- rlang::f_lhs(period)
  period_num <- check_num(period_num)

  # Check RHS
  period_char <- as.character(rlang::f_rhs(period))
  period_char <- parse_period(period_char)

  list(num = period_num, period = period_char)
}

