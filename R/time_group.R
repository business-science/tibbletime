#' Generate a vector of time-based groupings
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
time_group <- function(index, period = "yearly", start_date = NULL, ...) {

  # Early termination
  terminate <- terminate_early(index, period)
  if(terminate) {
    return(seq_len(length(index)))
  }

  # Min / max used to create series
  index_tib <- as_tbl_time(tibble::tibble(date = index), date)
  index_min <- min(index)
  index_max <- max(index)

  # Used to force the created series to be of the same class
  # Ex) POSIXct Even if `period` is higher periods like month/year
  index_class <- class(index)[1]

  # Time zone of the index
  tz <- attr(index, "tz")
  if(is.null(tz)) {
    tz <- Sys.timezone()
  }

  # Check and normalize group period
  period_list <- split_period(period)

  # If the start date is not missing, it is the start
  if (!is.null(start_date)) {

    assertthat::assert_that(as.POSIXct(start_date, tz = tz) <= index_min,
                            msg = "Start date must be less than index minimum")
    from <- start_date

  } else {

    # Otherwise floor the min
    from <- index_min %>%
      lubridate::floor_date(period_list[["period"]]) %>%
      as.character()
  }

  # Ceiling the max date
  to   <- index_max %>%
    lubridate::ceiling_date(period_list[["period"]]) %>%
    as.character()

  # Formularize
  from_to_f <- rlang::new_formula(from, to)

  # Create series
  #endpoint_dates <- create_series(from_to_f, period = period,
  #                                tz = tz, force_class = class(index)[1],
  #                                as_date_vector = TRUE)
  endpoint_dates_tbl <- create_series(from_to_f, period = period,
                                  tz = tz, force_class = class(index)[1]) %>%
    dplyr::mutate(.time_group=1)

  # days seem to be the most granular unit at this moment
  # since smaller units were early returned by terminate_early()
  #index_grid <-create_series(from_to_f, period = "days",
  #                     tz = tz, force_class = class(index)[1])

  # merge in period start indicators and create period numbers
  index_grid <- dplyr::full_join(index_tib, endpoint_dates_tbl, by="date")
  index_grid <- distinct(index_grid)
  index_grid <- arrange(index_grid, date)
  index_grid <-  dplyr::mutate(index_grid,.time_group=cumsum(!is.na(.time_group)))

  # Set initial names. NA for index, groups for endpoints
  # These become groups
  #names(index) <- NA_character_
  #names(endpoint_dates) <- seq_len(length(endpoint_dates))

  # Combine the two and sort
  # All the while keeping the groups as the names in the correct position
  #combined_dates <- c(endpoint_dates, index)

  #combined_dates_sorted <- sort(combined_dates)

  # Remove the names and convert to numeric
  #full_time_group <- as.numeric(names(combined_dates_sorted))

  # 'fill' the NA values forward with the correct group
  #not_na <- !is.na(full_time_group)
  #full_time_group <- cumsum(not_na)

  # Pull the endpoint_dates back out so we don't have duplicates
  # Match only finds the first match so this works correctly
  #.time_group <- full_time_group[-match(endpoint_dates, combined_dates_sorted)]
  # add dates as names
  #names(.time_group) <- as.character(combined_dates_sorted[-match(endpoint_dates, combined_dates_sorted)])
  # restore original order of the index using name subsetting and discard names
  #.time_group<-.time_group[as.character(index)]
  #names(.time_group) <- NULL

  # join the index to grid to match period number
  index_tib <- dplyr::left_join(index_tib, index_grid, by="date")
  .time_group <- dplyr::pull(index_tib, .time_group)

  # Subtract off min-1 (takes care of starting the groups too early)
  .time_group <- .time_group - (min(.time_group) - 1)

  .time_group
}

# Util -------------------------------------------------------------------------

# Decide whether to terminate early and return original data
terminate_early <- function(index, period) {

  # Originally don't terminate
  terminate <- FALSE

  # Split period, grab normalized period character
  period <- split_period(period)[[2]]

  # If it's a Date and sec/min/hour, yes terminate. No ability to change
  # periodicity
  if(inherits(index, "Date") & period %in% c("sec", "min", "hour")) {
    terminate <- TRUE
  }

  terminate
}
