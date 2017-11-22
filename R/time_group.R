#' Add time-based groupings to a tibble
#'
#' [time_group()] accepts a date index vector and returns an integer vector that
#'  can be used for grouping by periods.
#'
#' @param index A vector of date indices to create groups for.
#' @param period A formula or character specification used for time-based grouping.
#'
#'   If a formula, e.g. `1~year`, the formula is split and parsed to form the
#'   grouping period. The `period` argument accepts a formula of the form
#'   `multiple ~ period` allowing for flexible period grouping.
#'   The following are examples:
#'
#'   * 1 Year: `1~y`
#'   * 3 Months: `3~m`
#'   * 90 Days: `90~d`
#'
#'   Note that while shorthand is used above, an attempt is made to recognize
#'   more explicit period names such as:
#'
#'   * 2 Year: `2~year` / `2~years` / `2~yearly`
#'
#'   The `period` argument also accepts characters that are converted to their
#'   corresponding periods. The following are accepted:
#'
#'   * `"yearly"` or `"y"`
#'   * `"quarterly"` or `"q"`
#'   * `"monthly"` or `"m"`
#'   * `"weekly"` or `"w"`
#'   * `"daily"` or `"d"`
#'   * `"hour"` or `"h"`
#'   * `"minute"` or `"M"`
#'   * `"second"` or `"s"`
#'
#' @param start_date Optional argument used to specify the start date for the
#' first group. The default is to start at the closest period boundary
#' below the minimum date in the supplied index.
#' @param ... Not currently used.
#'
#' @details
#'
#' This function is used internally, but may provide the user extra flexibility
#' when they need to perform a grouped operation not supported by `tibbletime`.
#'
#' Grouping can only be done on the minimum periodicity of the index and above.
#' This means that a daily series cannot be grouped by minute. An hourly series
#' cannot be grouped by 5 seconds, and so on. If the user attempts this,
#' groups will be returned at the minimum periodicity (a daily series will
#' return 1 group per day).
#'
#' The `start_date` argument allows the user to control where the periods begin.
#'
#' This function respects [dplyr::group_by()] groups.
#'
#' @seealso [as_period()], [create_series()]
#'
#' @examples
#'
#' data(FB)
#'
#' time_group(FB$date, 2~y)
#'
#' dplyr::mutate(FB, time_group = time_group(date, 2~d))
#'
#' @export
#' @rdname time_group
time_group <- function(x, period = "yearly", start_date = NULL, ...) {
  UseMethod("time_group")
}

#' @export
time_group.default <- function(x, period = "yearly", start_date = NULL, ...) {
  stop("Object is not of class `tbl_time`.", call. = FALSE)
}

#' @export
time_group.tbl_time <- function(x, period = "yearly", start_date = NULL, ...) {

  index_quo <- get_index_quo(x)

  x_with_groups <- dplyr::mutate(
    .data = x,
    .time_group = make_time_group_vector(!! index_quo, period, start_date)
  )

  sloop::reconstruct(x_with_groups, x)
}


#' @export
#' @rdname time_group
make_time_group_vector <- function(index_col, period, start_date = NULL, ...) {

  .index_col      <- to_posixct_numeric(index_col)
  index_class     <- get_index_col_class(index_col)
  index_time_zone <- get_index_col_time_zone(index_col)

  # Check ordering of numeric index
  check_index_order(.index_col)

  # Parse the period
  period_list <- parse_period(period)

  # Generic validation of user defined period
  assert_period_matches_index_class(index_col, period_list$period)

  # Make endpoint time_formula
  endpoint_time_formula <- make_endpoint_formula(
    x = index_col,
    rounding_period = period_list$period,
    start_date = start_date
  )

  # Create series
  endpoints <- create_series(
    time_formula = endpoint_time_formula,
    period = period,
    class = index_class,
    tz = index_time_zone,
    as_vector = TRUE
  )

  endpoints <- to_posixct_numeric(endpoints)

  .time_group <- make_time_groups(.index_col, endpoints)

  .time_group
}

#### Utils ---------------------------------------------------------------------

make_endpoint_formula <- function(x, rounding_period, start_date = NULL) {
  # Get start_date
  if(is.null(start_date)) {
    start_date <- dplyr::first(x)

    # Auto start_date get's floored
    start_date <- start_date %>%
      floor_date_time(rounding_period)

  } else {
    # Coerce the user specified start_date
    start_date <- coerce_start_date(x, start_date)
    assert_start_date_before_index_min(x, start_date)
  }

  # Get end_date
  end_date <- dplyr::last(x) %>%
    ceiling_date_time(rounding_period)

  # As formula
  start_date ~ end_date
}


assert_start_date_before_index_min <- function(x, start_date) {
  assertthat::assert_that(
    to_posixct_numeric(dplyr::first(x)) >= to_posixct_numeric(start_date),
    msg = "start_date must be less than or equal to the minimum of the index column"
  )
}

make_time_groups <- function(x, endpoints) {
  # Combine the two and obtain the correct order
  combined_dates <- c(endpoints, x)
  sorted_order   <- order(combined_dates)

  # Create the unfilled time group vector and put it in the correct order
  endpoint_groups  <- rlang::seq2_along(1, endpoints)
  endpoint_fillers <- rep(NA, times = length(x))
  full_time_group <- c(endpoint_groups, endpoint_fillers)[sorted_order]

  # Remember location of endpoint_dates for removal later
  endpoint_locations <- match(endpoint_groups, full_time_group)

  # 'fill' the NA values forward with the correct group
  not_na <- !is.na(full_time_group)
  full_time_group <- cumsum(not_na)

  # Pull the endpoint_dates back out so we don't have duplicates
  .time_group <- full_time_group[-endpoint_locations]

  # Subtract off min-1 (takes care of starting the groups too early)
  .time_group <- .time_group - (.time_group[1] - 1L)
}

# Check if index in in ascending order, warn user if not.
check_index_order <- function(index) {

  if(!is.numeric(index)) {
    index <- as.numeric(index)
  }

  if(!is_ordered_numeric(index)) {
    message("Note: Index not ordered. tibbletime assumes index is in ascending order. Results may not be as desired.")
  }
}

