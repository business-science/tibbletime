#' Change `tbl_time` periodicity
#'
#' Convert a `tbl_time` object from daily to monthly,
#' from minute data to hourly, and more. This allows the user to easily
#' aggregate data to a less granular level by taking the value from either
#' the beginning or end of the period.
#'
#' @inheritParams partition_index
#' @param .tbl_time A `tbl_time` object.
#' @param side Whether to return the date at the beginning or the end of the
#' new period. By default, the `"start"` of the period. Use `"end"` to change
#' to the end of the period.
#' @param include_endpoints Whether to include the first or last data point in
#' addition to the transformed data.
#'
#' @details
#'
#' This function respects [dplyr::group_by()] groups.
#'
#' The `side` argument is useful when you want to return data at, say, the
#' end of a quarter, or the end of a month.
#'
#' `include_endpoints` can be useful when calculating a change over time.
#' In addition to changing to monthly dates, you often need the first data point
#' as a baseline for the first calculation.
#'
#'
#' @examples
#'
#' # Basic usage ---------------------------------------------------------------
#'
#' # FB stock prices
#' data(FB)
#' FB <- as_tbl_time(FB, date)
#'
#' # Aggregate FB to yearly data
#' as_period(FB, "yearly")
#'
#' # Aggregate FB to every 2 years
#' as_period(FB, "2 years")
#'
#' # Aggregate FB to yearly data, but use the last data point available
#' # in that period
#' as_period(FB, "yearly", side = "end")
#'
#' # Aggregate FB to yearly data, end of period, and include the first
#' # endpoint
#' as_period(FB, "yearly", side = "end", include_endpoints = TRUE)
#'
#' # Aggregate to weekly. Notice that it only uses the earliest day available
#' # in the data set at that periodicity. It will not set the date of the first
#' # row to 2013-01-01 because that date did not exist in the original data set.
#' as_period(FB, "weekly")
#'
#' # Aggregate to every other week
#' as_period(FB, "2 weeks")
#'
#' # FB is daily data, aggregate to minute?
#' # Not allowed for Date class indices, an error is thrown
#' # as_period(FB, "minute")
#'
#' # Grouped usage -------------------------------------------------------------
#'
#' # FANG contains Facebook, Amazon, Netflix and Google stock prices
#' data(FANG)
#' FANG <- as_tbl_time(FANG, date)
#'
#' FANG <- dplyr::group_by(FANG, symbol)
#'
#' # Respects groups
#' as_period(FANG, "yearly")
#'
#' # Every 6 months, respecting groups
#' as_period(FANG, "6 months")
#'
#' # Using start_date ----------------------------------------------------------
#'
#' # WARNING: The start_date argument is being retired in 0.1.2 favor of the more
#' # flexible option of passing an index vector to the period argument. See
#' # below for a comparison of the two methods.
#'
#' #### Old method using start_date
#'
#' # FB stock prices
#' data(FB)
#' FB <- as_tbl_time(FB, date)
#'
#' # The Facebook series starts at 2013-01-02 so the 'every 2 day' counter
#' # starts at that date as well. Groups become (2013-01-02, 2013-01-03),
#' # (2013-01-04, 2013-01-05) and so on.
#' as_period(FB, "2 day")
#'
#' # Specifying the `start_date = "2013-01-01"` might be preferable.
#' # Groups become (2013-01-01, 2013-01-02), (2013-01-03, 2013-01-04) and so on.
#' as_period(FB, "2 day", start_date = "2013-01-01")
#'
#' #### New method using an index vector
#'
#' # FB stock prices
#' data(FB)
#' FB <- as_tbl_time(FB, date)
#'
#' custom_period <- create_series(
#'   time_formula = dplyr::first(FB$date) - 1 ~ dplyr::last(FB$date),
#'   period       = "2 day",
#'   class        = "Date",
#'   as_vector    = TRUE)
#'
#' FB %>%
#'   as_tbl_time(date) %>%
#'   as_period(period = custom_period)
#'
#' # Manually calculating returns at different periods -------------------------
#'
#' data(FB)
#'
#' # Annual Returns
#' # Convert to end of year periodicity, but include the endpoints to use as
#' # a reference for the first return calculation. Then calculate returns.
#' FB %>%
#'   as_tbl_time(date) %>%
#'   as_period("1 y", side = "end", include_endpoints = TRUE) %>%
#'   dplyr::mutate(yearly_return = adjusted / dplyr::lag(adjusted) - 1)
#'
#' @export
#'
as_period <- function(.tbl_time, period = "yearly",
                      start_date = NULL, side = "start",
                      include_endpoints = FALSE, ...) {
  UseMethod("as_period")
}

#' @export
as_period.default <- function(.tbl_time, period = "yearly",
                              start_date = NULL, side = "start",
                              include_endpoints = FALSE, ...) {
  stop("Object is not of class `tbl_time`.", call. = FALSE)
}

#' @export
as_period.tbl_time <- function(.tbl_time, period = "yearly",
                               start_date = NULL, side = "start",
                               include_endpoints = FALSE, ...) {

  # Add time groups
  .tbl_time_tg <- dplyr::mutate(
    .tbl_time,
    .time_group = partition_index(!! get_index_quo(.tbl_time), period, start_date)
  )

  # Filter
  if(side == "start")
    .tbl_time_tg <- dplyr::filter(
      .tbl_time_tg,
      {
        .tg <- .data$.time_group
        criteria <- vector(length = length(.tg))
        criteria[match(unique(.tg), .tg)] <- TRUE

        # Include last end point
        if(include_endpoints) {
          criteria[length(criteria)] <- TRUE
        }

        criteria
      }
    )
  else if(side == "end") {
    .tbl_time_tg <- dplyr::filter(
      .tbl_time_tg,
      {
        .tg <- .data$.time_group
        criteria <- vector(length = length(.tg))
        criteria[length(.tg) - match(unique(.tg), rev(.tg)) + 1] <- TRUE

        # Include first end point
        if(include_endpoints) {
          criteria[1] <- TRUE
        }

        criteria
      }
    )
  }

  # Remove time group column
  .tbl_time_tg <- remove_time_group(.tbl_time_tg)

  reconstruct(.tbl_time_tg, .tbl_time)
}


