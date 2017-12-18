#' Collapse a `tbl_time` object so that all observations in a period share the
#' same date
#'
#' When `time_collapse` is used, the index of a `tbl_time` object is altered
#' so that all dates that fall in a period share a common date. This can be
#' useful for further groupwise calculations.
#'
#' @inheritParams partition_index
#' @param .data A `tbl_time` object.
#' @param as_sep_col Whether to keep the original index as the column `.date`
#' or to drop it.
#' @param ... Not currently used.
#'
#' @details
#'
#' The date chosen as the common date for a period is always the date
#' at the _end_ of that period.
#'
#' It is often useful to use `as_sep_col = TRUE` to keep the original dates
#' as well.
#'
#' This function respects [dplyr::group_by()] groups.
#'
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
#' time_collapse(FB, period = "weekly")
#'
#' # Collapse to every other week dates
#' time_collapse(FB, period = 2~w)
#'
#' # Collapse to weekly dates, but keep the original too
#' time_collapse(FB, period = "weekly", as_sep_col = TRUE)
#'
#' # Grouped functionality -----------------------------------------------------
#'
#' data(FANG)
#' FANG <- FANG %>%
#'   as_tbl_time(date) %>%
#'   dplyr::group_by(symbol)
#'
#' # Collapse each group to monthly
#' FANG %>%
#'   time_collapse("monthly")
#'
#' # Collapse each group to every other month
#' FANG %>%
#'   time_collapse(2~m)
#'
#' @export
#'
collapse_index <- function(index, period = "yearly", start_date = NULL, where = "start", ...) {

  # Partition index
  index_part <- partition_index(index, period, start_date)

  # Index as numeric (unlist would remove attrs)
  index_num <- to_posixct_numeric(index)

  if(where == "start") {
    # For each partition, find the first date position
    pos <- match(unique(index_part), index_part)
    index_at_pos <- index_num[pos]

    # Each date must be repeated this many times to rebuild the column
    reps <- diff(c(pos, length(index_part) + 1))

  }
  else if(where == "end") {
    # For each partition, find the last date position
    pos <- length(index_part) - match(unique(index_part), rev(index_part)) + 1
    index_at_pos <- index_num[pos]

    # Each date must be repeated this many times to rebuild the column
    reps <- diff(c(0, pos))

  }

  new_index <- rep(index_at_pos, reps) %>%
    posixct_numeric_to_datetime(class = get_index_col_class(index), tz = get_index_col_time_zone(index))

  new_index
}
