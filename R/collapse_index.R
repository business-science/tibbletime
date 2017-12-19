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
#' @param ... Not currently used.
#'
#' @details
#'
#' Because this is often used for end of period summaries, the default is to
#' use `side = "end"`. Note that this is the opposite of [as_period()] where
#' the default is `side = "start"`.
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
#' # A common workflow is the group on the new date column
#' # to perform a time based summary
#' FB %>%
#'  dplyr::mutate(date = collapse_index(date, "yearly")) %>%
#'  dplyr::group_by(date) %>%
#'  dplyr::summarise_if(is.numeric, mean)
#'
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
                           start_date = NULL, side = "end", ...) {

  # Partition index
  index_part <- partition_index(index, period, start_date)

  # Index as numeric (unlist would remove attrs)
  index_num <- to_posixct_numeric(index)

  if(side == "start") {
    # For each partition, find the first date position
    pos <- match(unique(index_part), index_part)
    index_at_pos <- index_num[pos]

    # Each date must be repeated this many times to rebuild the column
    reps <- diff(c(pos, length(index_part) + 1))

  }
  else if(side == "end") {
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

  new_index
}
