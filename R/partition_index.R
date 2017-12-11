#' Add time-based groupings to a tibble
#'
#' [partition_index()] accepts a date index vector and returns an integer vector that
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
#' partition_index(FB$date, 2~y)
#'
#' dplyr::mutate(FB, partition_index = partition_index(date, 2~d))
#'
#' @export
#' @rdname partition_index
#'
partition_index <- function(index, period = "yearly", start_date = NULL, ...) {

   make_partition_index_vector(index, period, start_date, ...)

}


#' @export
#' @rdname partition_index
make_partition_index_vector <- function(index_col, period, start_date = NULL, ...) {

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
    index = index_col,
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

  .partition_index <- make_partition_index(.index_col, endpoints)

  .partition_index
}

#### Utils ---------------------------------------------------------------------

make_endpoint_formula <- function(index, rounding_period, start_date = NULL) {
  # Get start_date
  if(is.null(start_date)) {
    start_date <- dplyr::first(index)

    # Auto start_date get's floored
    start_date <- start_date %>%
      floor_index(rounding_period)

  } else {
    # Coerce the user specified start_date
    start_date <- coerce_start_date(index, start_date)
    assert_start_date_before_index_min(index, start_date)
  }

  # Get end_date
  end_date <- dplyr::last(index) %>%
    ceiling_index(rounding_period)

  # As formula
  start_date ~ end_date
}


assert_start_date_before_index_min <- function(index, start_date) {
  assertthat::assert_that(
    to_posixct_numeric(dplyr::first(index)) >= to_posixct_numeric(start_date),
    msg = "start_date must be less than or equal to the minimum of the index column"
  )
}

make_partition_index <- function(index, endpoints) {
  # Combine the two and obtain the correct order
  combined_dates <- c(endpoints, index)
  sorted_order   <- order(combined_dates)

  # Create the unfilled time group vector and put it in the correct order
  endpoint_groups  <- rlang::seq2_along(1, endpoints)
  endpoint_fillers <- rep(NA, times = length(index))
  full_partition_index <- c(endpoint_groups, endpoint_fillers)[sorted_order]

  # Remember location of endpoint_dates for removal later
  endpoint_locations <- match(endpoint_groups, full_partition_index)

  # 'fill' the NA values forward with the correct group
  not_na <- !is.na(full_partition_index)
  full_partition_index <- cumsum(not_na)

  # Pull the endpoint_dates back out so we don't have duplicates
  .partition_index <- full_partition_index[-endpoint_locations]

  # Subtract off min-1 (takes care of starting the groups too early)
  .partition_index <- .partition_index - (.partition_index[1] - 1L)
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

