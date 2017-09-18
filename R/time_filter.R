#' Succinctly filter a `tbl_time` object by date
#'
#' Use a concise filtering method to filter for rows where the `index`
#' falls within a date range
#'
#' @details
#'
#' The `time_formula` is specified using the format `from ~ to`.
#' Each side of the `time_formula` is specified as `YYYY-MM-DD + HH:MM:SS`,
#' but powerful shorthand is available. Some examples are:
#' * __Year:__ `2013 ~ 2015`
#' * __Month:__ `2013-01 ~ 2016-06`
#' * __Day:__ `2013-01-05 ~ 2016-06-04`
#' * __Second:__ `2013-01-05 + 10:22:15 ~ 2018-06-03 + 12:14:22`
#' * __Variations:__ `2013 ~ 2016-06`
#'
#' The `time_formula` can also use a one sided formula.
#' * __Only dates in 2015:__ `~2015`
#' * __Only dates March 2015:__ `~2015-03`
#'
#' All shorthand dates are expanded to be valid POSIXct dates.
#' * The `from` side is expanded to be the first date in that period
#' * The `to` side is expanded to be the last date in that period
#'
#' This means that the following examples are equivalent:
#' * `2015 ~ 2016 == 2015-01-01 + 00:00:00 ~ 2016-12-31 + 23:59:59`
#' * `~2015 == 2015-01-01 + 00:00:00 ~ 2015-12-31 + 23:59:59`
#' * `2015-01-04 + 10:12 ~ 2015-01-05 == 2015-01-04 + 10:12:00 ~ 2015-01-05 + 23:59:59`
#'
#' This function respects [dplyr::group_by()] groups.
#'
#' @param x A `tbl_time` object.
#' @param time_formula A period to filter over. This is specified as a `formula`.
#'
#' @rdname time_filter
#'
#' @export
#'
#' @examples
#'
#' # FANG contains Facebook, Amazon, Netflix and Google stock prices
#' data(FANG)
#' FANG <- as_tbl_time(FANG, date) %>%
#'   group_by(symbol)
#'
#' # 2013-01-01 to 2014-12-31
#' time_filter(FANG, 2013 ~ 2014)
#'
#' # 2013-05-25 to 2014-06-04
#' time_filter(FANG, 2013-05-25 ~ 2014-06-04)
#'
#' # Using the `[` subset operator
#' FANG[2014~2015]
#'
#' # Using `[` and one sided formula for only dates in 2014
#' FANG[~2014]
#'
#' # Using `[` and column selection
#' FANG[2013~2016, c("date", "adjusted")]
#'
#'
time_filter <- function(x, time_formula) {
  UseMethod("time_filter")
}

#' @export
time_filter.default <- function(x, time_formula) {
  stop("Object is not of class `tbl_time`.", call. = FALSE)
}

#' @export
time_filter.tbl_time <- function(x, time_formula) {

  # Validate time_formula syntax
  from_to <- formula_to_char(time_formula)

  # Index name as sym
  index_name <- rlang::sym(retrieve_index(x, as_name = TRUE))
  index_raw      <- retrieve_index(x) %>%
    dplyr::pull()

  # Normalize
  from_to_clean <- purrr::map2_chr(.x = from_to,
                                   .y = c("from", "to"),
                                   .f = normalize_date)

  # Validate time_formula date order
  validate_date_order(from = from_to_clean[1], to = from_to_clean[2])

  # Date function selection
  date_fun <- date_fun_selector(x)

  # Filter for those rows
  dplyr::filter(x,
                rlang::UQ(index_name) >= date_fun(from_to_clean[1],
                                                    tz = retrieve_time_zone(x)),
                rlang::UQ(index_name) <= date_fun(from_to_clean[2],
                                                    tz = retrieve_time_zone(x)))

}

# Subset operator --------------------------------------------------------------

#' @export
#'
#' @param i A period to filter over. This is specified the same as
#' `time_formula` but this follows the normal extraction argument syntax.
#' @param j Optional argument to also specify column index to subset. Works
#' exactly like the normal extraction operator.
#' @param drop Will always be coerced to `FALSE` by `tibble`.
#'
#' @rdname time_filter
#'
`[.tbl_time` <- function(x, i, j, drop = FALSE) {

  # Classes and attributes to keep
  time_classes <- stringr::str_subset(class(x), "tbl_time")
  time_attrs <- list(
    index     = attr(x, "index"),
    time_zone = attr(x, "time_zone")
  )

  # This helps decide whether i is used for column subset or row subset
  .nargs <- nargs() - !missing(drop)

  # time_filter if required
  if(!missing(i)) {
    if(rlang::is_formula(i)) {
      x <- time_filter(x, i)
    }
  }

  # detime
  x <- detime(x, time_classes, time_attrs)

  # i filter
  if(!missing(i)) {
    if(!rlang::is_formula(i)) {
      if(.nargs <= 2) {
        # Column subset
        # Preferred if tibble issue is addressed
        # x <- x[i, drop = drop]
        x <- x[i]
      } else {
        # Row subset
        x <- x[i, , drop = drop]
      }

    }
  }

  # j filter
  if(!missing(j)) {
    x <- x[, j, drop = drop]
  }

  # retime
  retime(x, time_classes, time_attrs)
}


# Util -------------------------------------------------------------------------

# Check a user supplied time_formula for correct syntax
# If "2015", duplicate to "2015,2015"
# Removes leading / trailing spaces
formula_to_char <- function(time_formula) {

  # Must be a formula
  assertthat::assert_that(rlang::is_formula(time_formula),
                          msg = "Period must be specified as a formula using `~`")

  # Split dates, remove ~
  time_formula <- as.character(time_formula)[-1]

  # Remove spaces
  time_formula <- gsub(" ", "", time_formula)

  # If rhs only, duplicate
  if(length(time_formula) == 1) {
    time_formula <- c(time_formula, time_formula)
  }

  # Check symbols
  check_syms <- function(x) {
    assertthat::assert_that(stringr::str_count(x, "-") <= 2,
                            msg = "A maximum of two '-' are possible per date")
    assertthat::assert_that(stringr::str_count(x, ":") <= 2,
                            msg = "A maximum of two ':' are possible per date")
    assertthat::assert_that(stringr::str_count(x, "\\+") <= 1,
                            msg = "A maximum of one '+' is possible per date")
    assertthat::assert_that(!stringr::str_detect(x, "^:|:$|\\s:|:\\s"),
                            msg = "A ':' can only be used between two numbers")
    assertthat::assert_that(!stringr::str_detect(x, "^-|-$|\\s-|-\\s"),
                            msg = "A '-' can only be used between two numbers")
  }
  lapply(time_formula, check_syms)

  time_formula
}

# Validates the final dates
# `from` must be before `to`
validate_date_order <- function(from, to) {
  from <- as.POSIXct(from)
  to   <- as.POSIXct(to)

  assertthat::assert_that(from <= to, msg = "`from` must be a date before `to`")
}

# Expand date shorthand into a real date
normalize_date <- function(x, from_to) {

  # Setup ymd / hms lists to fill
  ymd <- switch(from_to,
                "from" = list(y = "1970", m = "01", d = "01"),
                # `to` day depends on month selected, filled later if necessary
                "to"   = list(y = "1970", m = "12", d = "00"))

  hms <- switch(from_to,
                "from" = list(h = "00", m = "00", s = "00"),
                "to"   = list(h = "23", m = "59", s = "59"))

  # Check existance of date / time dividing space \\s
  date_time <- if(stringr::str_detect(x, "\\+")) {

    # If there is a date and a time, split them
    date_time <- unlist(stringr::str_split(x, "\\+"))

    # Recurse split to fill the lists
    date <- recurse_split(date_time[1], ymd, "-")
    time <- recurse_split(date_time[2], hms, ":")

    # Paste together
    paste(date, time, sep = " ")
  } else {

    # If there is only a date, no time
    # Recurse split the date, and pass 0 as time
    date <- recurse_split(x, ymd, "-")
    hms_default <- paste(unlist(hms), collapse = ":")
    time <- recurse_split(hms_default, hms, ":")

    # Paste together
    paste(date, time, sep = " ")
  }

  date_time
}

# Split x by the splitter and fill the filler list with the pieces
recurse_split <- function(x, filler, splitter) {
  i <- 1

  # While there is something to split on
  while(stringr::str_detect(x, splitter)) {

    # Extract the first part of the split
    piece <- stringr::str_extract(x, paste0("([^", splitter, "]+)"))

    # Replace the first part with "" in the string
    x <- stringr::str_replace(x,
                              pattern = paste0("([^", splitter, "]+", splitter, ")"),
                              replacement = "")

    # Add the new piece to the filler
    filler[[i]] <- piece

    # Next
    i <- i + 1
  }
  # Once there is nothing left to split on, add the last piece to the filler
  filler[[i]] <- x

  # If `to` d was never changed, set as end of chosen month
  if(!is.null(filler[["d"]])) {
    if(filler[["d"]] == "00") {

      # Fake a date to find the number of days in that month
      filler[["d"]] <- "01"
      fake_date <- as.Date(paste0(unlist(filler), collapse = splitter))

      # Fill the `to` d with the number of days in that month
      filler[["d"]] <- as.character(lubridate::days_in_month(fake_date))
    }
  }

  paste0(unlist(filler), collapse = splitter)
}

# Set date conversion function
date_fun_selector <- function(x) {

  index_raw <- retrieve_index(x) %>%
    dplyr::pull()

  # Switch based on class of the index
  if(inherits(index_raw, "Date")) {
    as.Date
  } else if(inherits(index_raw, "POSIXct")) {
    as.POSIXct
  } else {
    as.Date
  }
}
