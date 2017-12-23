#' Change `tbl_time` periodicity
#'
#' Convert a `tbl_time` object from daily to monthly,
#' from minute data to hourly, and more. This allows the user to easily
#' aggregate data by taking the value from either the beginning or  
#' the end of the period.  Use `time_summarise` to apply function
#' to time period's values.
#'
#' @inheritParams time_group
#' @param x A `tbl_time` object.
#' @param side Whether to return the value for the date at the beginning or the end of the
#' new period. By default, the `"start"` of the period. Use `"end"` to change
#' to the end of the period.
#'
#' @details
#'
#' This function respects [dplyr::group_by()] groups.
#'
#' Currently periods finer than second data are not supported.
#'
#' The `side` argument is useful when you want to return data at, say, the
#' end of a quarter, or the end of a month.
#'
#'
#' @export
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
#' # Aggregate FB to yearly data using a period formula
#' as_period(FB, 1~y)
#'
#' # Aggregate FB to yearly data, but use the last data point available
#' # in that period
#' as_period(FB, "yearly", "end")
#'
#' # Aggregate to weekly. Notice that it only uses the earliest day available
#' # in the data set at that periodicity. It will not set the date of the first
#' # row to 2013-01-01 because that date did not exist in the original data set.
#' as_period(FB, "weekly")
#'
#' # Aggregate to every other week
#' as_period(FB, 2~w)
#'
#' # FB is daily data, aggregate to minute?
#' # Does nothing and returns the original data set.
#' as_period(FB, "minute")
#'
#' # Grouped usage -------------------------------------------------------------
#'
#' # FANG contains Facebook, Amazon, Netflix and Google stock prices
#' data(FANG)
#' FANG <- as_tbl_time(FANG, date)
#'
#' FANG <- FANG %>% dplyr::group_by(symbol)
#'
#' # Respects groups
#' FANG %>% as_period("yearly")
#'
#' # Every 6 months, respecting groups
#' as_period(FANG, 6~m)
#'
#' # Using start_date ----------------------------------------------------------
#'
#' # FB stock prices
#' data(FB)
#' FB <- as_tbl_time(FB, date)
#'
#' # The Facebook series starts at 2013-01-02 so the 'every 2 day' counter
#' # starts at that date as well. Groups become (2013-01-02, 2013-01-03),
#' # (2013-01-04, 2013-01-05) and so on.
#' as_period(FB, 2~d)
#'
#' # Specifying the `start_date = "2013-01-01"` might be preferable.
#' # Groups become (2013-01-01, 2013-01-02), (2013-01-03, 2013-01-04) and so on.
#' as_period(FB, 2~d, start_date = "2013-01-01")
#'
as_period <- function(x, period = "yearly",
                      side = "start", start_date = NULL) {
  UseMethod("as_period")
}

#' @export
as_period.default <- function(x, period = "yearly",
                              side = "start", start_date = NULL) {
  stop("Object is not of class `tbl_time`.", call. = FALSE)
}

#' @export
as_period.tbl_time <- function(x, period = "yearly",
                               side = "start", start_date = NULL) {

  # Index tibble/sym
  index_name <- rlang::sym(retrieve_index(x, as_name = TRUE))

  # Add time group column
  x <- dplyr::mutate(x, .time_group = time_group(!! index_name,
                                                 period = period,
                                                 start_date = start_date))

  # Select the max or min row of each group
  .tg <- x[[".time_group"]]
  .tg_unique <- unique(.tg)

  if(side == "start") {
    # Match the first time group value in each group
    x <- x[match(.tg_unique, .tg),]
  } else if (side == "end") {
    # Match the last time group value in each group
    x <- x[length(.tg) - match(.tg_unique, rev(.tg)) + 1,]
  }

  # Remove time group column
  x[[".time_group"]] <- NULL

  x
}

#' @export
as_period.grouped_tbl_time <- function(x, period = "yearly",
                                       side = "start", start_date = NULL) {

  # Nest and apply to each group
  x %>%
    tidyr::nest() %>%
    dplyr::mutate(data = purrr::map(.data$data,
                                    ~as_period(.x,
                                               period = period,
                                               side = side,
                                               start_date = start_date))) %>%
    time_unnest() %>%
    dplyr::group_by(!!! dplyr::groups(x))
}

