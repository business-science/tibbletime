#' Partition an index vector into an integer vector representing groups
#'
#' [partition_index()] takes an index vector and returns an integer vector that
#' can be used for grouping by periods. This is the workhorse for many other
#' `tibbletime` functions.
#'
#' @param index A vector of date indices to create groups for.
#' @param period A character specification used for time-based grouping. The
#' general format to use is `"frequency period"` where frequency is a number
#' like 1 or 2, and period is an interval like weekly or yearly. There must be
#' a space between the two.
#'
#'   Note that you can pass the specification in a flexible way:
#'
#'   * 1 Year: `'1 year'` / `'1 Y'`
#'
#'   This shorthand is available for year, quarter, month, day, hour, minute,
#'   second, millisecond and microsecond periodicities.
#'
#'   Additionally, you have the option of passing in a vector of dates to
#'   use as custom and more flexible boundaries.
#'
#' @param start_date Optional argument used to
#' specify the start date for the
#' first group. The default is to start at the closest period boundary
#' below the minimum date in the supplied index.
#' @param ... Not currently used.
#'
#' @details
#'
#' This function is used internally, but may provide the user extra flexibility
#' in some cases.
#'
#' Grouping can only be done on the minimum periodicity of the index and above.
#' This means that a daily series cannot be grouped by minute. An hourly series
#' cannot be grouped by 5 seconds, and so on. If the user attempts this,
#' an error will be thrown.
#'
#' @seealso [as_period()], [collapse_index()]
#'
#' @examples
#'
#' data(FB)
#'
#' partition_index(FB$date, '2 year')
#'
#' dplyr::mutate(FB, partition_index = partition_index(date, '2 day'))
#'
#' @export
#' @rdname partition_index
#'
partition_index <- function(index, period = "year", start_date = NULL, ...) {

  .index <- to_posixct_numeric(index)

  # Check ordering of numeric index
  check_index_order(.index)

  # Find the correct boundaries for the partitioned index
  endpoints <- make_endpoints(index, period, start_date)

  # Use the boundaries to break up the index
  make_partitioned_index(.index, endpoints)
}

#### Utils ---------------------------------------------------------------------

# Create the break points from the user's index and their specified period
# to break on
make_endpoints <- function(index, period, start_date) {

  # Allow the user to pass in an index vector to be used as the period
  if(inherits_allowed_datetime(period)) {

    assert_custom_period_class_matches_index_class(index, period)
    endpoints <- period

  # Otherwise, parse the period and make an endpoint vector
  } else {

    index_class     <- get_index_col_class(index)
    index_time_zone <- get_index_col_time_zone(index)

    # Parse the period
    period_list <- parse_period(period)

    # Generic validation of user defined period
    assert_period_matches_index_class(index, period_list$period)

    # Make endpoint time_formula
    period_clean <- paste(period_list[["freq"]], period_list[["period"]])

    endpoint_time_formula <- make_endpoint_formula(
      index = index,
      period = period_clean,
      start_date = start_date
    )

    # Create series
    endpoints <- create_series(
      time_formula = endpoint_time_formula,
      period = period,
      class = index_class,
      tz = index_time_zone,
      include_end = TRUE,
      as_vector = TRUE
    )

  }

  endpoints <- to_posixct_numeric(endpoints)

  endpoints
}

make_endpoint_formula <- function(index, period, start_date = NULL) {
  # Get start_date
  if(is.null(start_date)) {
    start_date <- dplyr::first(index)

    # Auto start_date get's floored (only by the period)
    start_date <- floor_index(start_date, period)

  } else {
    # Coerce the user specified start_date
    start_date <- coerce_start_date(index, start_date)
    assert_start_date_before_index_min(index, start_date)
  }

  # Get end_date (ceilinged by the full period to have the correct last grouping)
  end_date <- ceiling_index(dplyr::last(index), period)

  # As formula
  start_date ~ end_date
}


assert_start_date_before_index_min <- function(index, start_date) {
  assertthat::assert_that(
    to_posixct_numeric(dplyr::first(index)) >= to_posixct_numeric(start_date),
    msg = "start_date must be less than or equal to the minimum of the index column"
  )
}

make_partitioned_index <- function(index, endpoints) {
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

  .partition_index
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

assert_custom_period_class_matches_index_class <- function(index, period) {
  assertthat::assert_that(
    get_index_col_class(index) == get_index_col_class(period),
    msg = "The custom period vector class much match the class of the index"
  )
}
