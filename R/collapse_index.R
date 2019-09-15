#' Collapse an index vector so that all observations in an interval share the
#' same date
#'
#' When `collapse_index()` is used, the index vector is altered
#' so that all dates that fall in a specified interval share a common date.
#' The most common use case for this is to then group on the collapsed index.
#'
#' @inheritParams partition_index
#' @param index An index vector.
#' @param side Whether to return the date at the beginning or the end of
#' the new period. By default, the "end" of the period.
#' Use "start" to change to the start of the period.
#' @param clean Whether or not to round the collapsed index up / down to the next
#' period boundary. The decision to round up / down is controlled by the side
#' argument.
#' @param ... Not currently used.
#'
#' @details
#'
#' The [collapse_by()] function provides a shortcut for the most common use
#' of `collapse_index()`, calling the function inside a call to `mutate()` to
#' modify the index directly. For more flexibility, like the nesting example
#' below, use `collapse_index()`.
#'
#' Because this is often used for end of period summaries, the default is to
#' use `side = "end"`. Note that this is the opposite of [as_period()] where
#' the default is `side = "start"`.
#'
#' The `clean` argument is especially useful if you have an irregular series
#' and want cleaner dates to report for summary values.
#'
#' @examples
#'
#' # Basic functionality -------------------------------------------------------
#'
#' # Facebook stock prices
#' data(FB)
#' FB <- as_tbl_time(FB, date)
#'
#' # Collapse to weekly dates
#' dplyr::mutate(FB, date = collapse_index(date, "weekly"))
#'
#' # A common workflow is to group on the new date column
#' # to perform a time based summary
#' FB %>%
#'   dplyr::mutate(date = collapse_index(date, "yearly")) %>%
#'   dplyr::group_by(date) %>%
#'   dplyr::summarise_if(is.numeric, mean)
#'
#' # You can also assign the result to a separate column and use that
#' # to nest on, allowing for 'period nests' that keep the
#' # original dates in the nested tibbles.
#' FB %>%
#'   dplyr::mutate(nest_date = collapse_index(date, "2 year")) %>%
#'   dplyr::group_by(nest_date) %>%
#'   tidyr::nest()
#'
#' # Grouped functionality -----------------------------------------------------
#'
#' data(FANG)
#' FANG <- FANG %>%
#'   as_tbl_time(date) %>%
#'   dplyr::group_by(symbol)
#'
#' # Collapse each group to monthly,
#' # calculate monthly standard deviation for each column
#' FANG %>%
#'   dplyr::mutate(date = collapse_index(date, "monthly")) %>%
#'   dplyr::group_by(date, add = TRUE) %>%
#'   dplyr::summarise_all(sd)
#'
#'
#' @export
#'
collapse_index <- function(index, period = "yearly",
                           start_date_for_collapse = NULL, side = "end", clean = FALSE, ...) {

  # Side either start or end
  assert_valid_side(side)

  # Index as numeric (unlist would remove attrs)
  index_num <- to_posixct_numeric(index)

  # Very different approach with clean. Only returning the endpoints
  # filled to the correct location, not using the user's indices.
  if(clean) {

    # Create datetime endpoints
    endpoints <- make_endpoints(index, period, start_date_for_collapse)

    # Create a numeric index containing the endpoints positioned in a way
    # to replace the old index
    new_numeric_index <- make_partitioned_endpoints(index_num, endpoints, side)

    # Convert to datetime
    new_index <- posixct_numeric_to_datetime(
      x     = new_numeric_index,
      class = get_index_col_class(index),
      tz    = get_index_col_time_zone(index)
    )

  # Else do a standard partition using the user's indices as the endpoints
  } else {

    # Partition index
    index_part <- partition_index(index, period, start_date_for_collapse)

    if(side == "start") {
      # For each partition, find the first date position
      pos <- match(unique(index_part), index_part)
      index_at_pos <- index_num[pos]

      # Each date must be repeated this many times to rebuild the column
      reps <- diff(c(pos, length(index_part) + 1))

    } else if(side == "end") {
      # For each partition, find the last date position
      pos <- length(index_part) - match(unique(index_part), rev(index_part)) + 1
      index_at_pos <- index_num[pos]

      # Each date must be repeated this many times to rebuild the column
      reps <- diff(c(0, pos))

    }

    new_index <- posixct_numeric_to_datetime(
      x     = rep(index_at_pos, reps),
      class = get_index_col_class(index),
      tz    = get_index_col_time_zone(index)
    )

  }

  new_index
}


#' Collapse a tbl_time object by its index
#'
#' Collapse the index of a `tbl_time` object by time period. The index column
#' is altered so that all dates that fall in a specified interval share a
#' common date.
#'
#' @inheritParams collapse_index
#' @inheritParams as_period
#'
#' @details
#'
#' `collapse_by()` is a simplification of a call to [dplyr::mutate()] to collapse an
#' index column using [collapse_index()].
#'
#' @examples
#'
#' # Basic functionality -------------------------------------------------------
#'
#' # Facebook stock prices
#' data(FB)
#' FB <- as_tbl_time(FB, date)
#'
#' # Collapse to weekly dates
#' collapse_by(FB, "weekly")
#'
#' # A common workflow is to group on the collapsed date column
#' # to perform a time based summary
#' FB %>%
#'   collapse_by("yearly") %>%
#'   dplyr::group_by(date) %>%
#'   dplyr::summarise_if(is.numeric, mean)
#'
#' # Grouped functionality -----------------------------------------------------
#'
#' data(FANG)
#' FANG <- FANG %>%
#'   as_tbl_time(date) %>%
#'   dplyr::group_by(symbol)
#'
#' # Collapse each group to monthly,
#' # calculate monthly standard deviation for each column
#' FANG %>%
#'   collapse_by("monthly") %>%
#'   dplyr::group_by(date, add = TRUE) %>%
#'   dplyr::summarise_all(sd)
#'
#' @export
collapse_by <- function(.tbl_time, period = "yearly", start_date_for_collapse = NULL, side = "end", clean = FALSE, ...) {

  index_quo  <- get_index_quo(.tbl_time)
  index_char <- get_index_char(.tbl_time)

  .tbl_time_collapsed <- dplyr::mutate(
    .data = .tbl_time,
    !! index_char := collapse_index(
      index      = !! index_quo,
      period     = period,
      start_date_for_collapse = start_date_for_collapse,
      side       = side,
      clean      = clean,
      ...
    )
  )

  .tbl_time_collapsed
}


assert_valid_side <- function(side) {
  assertthat::assert_that(
    side %in% c("start", "end"),
    msg = "`side` must be either 'start' or 'end'"
  )
}

# This is similar to make_partitioned_index but rather than returning
# a vector of integers corresponding to groups it returns a vector
# of endpoints corresponding to the groups. These endpoints are 'clean'
# dates
make_partitioned_endpoints <- function(index, endpoints, side = "end") {
  # Combine the two and obtain the correct order
  combined_dates <- c(endpoints, index)
  sorted_order   <- order(combined_dates)

  # Create the unfilled time group vector and put it in the correct order
  endpoint_fillers <- rep(NA, times = length(index))
  full_partition_index <- c(endpoints, endpoint_fillers)[sorted_order]

  # Remember location of endpoint dates for removal later
  endpoint_locations <- match(endpoints, full_partition_index)

  # 'fill' the NA values forward/backward with the correct endpoint
  from_last <- ifelse(side == "end", yes = TRUE, no = FALSE)
  full_partition_index <- zoo::na.locf0(full_partition_index, fromLast = from_last)

  # Pull the endpoints back out so we don't have duplicates
  .partition_index <- full_partition_index[-endpoint_locations]

  .partition_index
}
