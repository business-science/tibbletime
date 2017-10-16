#' Collapse a `tbl_time` object so that all observations in a period share the
#' same date
#'
#' When `time_collapse` is used, the index of a `tbl_time` object is altered
#' so that all dates that fall in a period share a common date. This can be
#' useful for further groupwise calculations.
#'
#' @inheritParams time_group
#' @param .data A `tbl_time` object.
#' @param as_sep_col Whether to keep the original index as the column `.date`
#' or to drop it.
#' @param normalize Whether to discard lower levels of detail in the date/time
#' infomation of the index up to the level corresponding to `period`.
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
#' When function is applied, observations falling into the same period will share
#' an index value, corresponding to the latest observation found in the original
#' index for each period. Specifying `normalize = TRUE` will reset lower levels
#' of detail in the index to the "default" value, e.g. if `period` is set to
#' "monthly", all dates in the index would be reset to the 1st of the month and
#' time information, if any, will be set to 00:00:00. This may be useful for
#' joining datasets of different date/time frequencies.
#'
#' The function respects [dplyr::group_by()] groups.
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
#' # Collapse to monthly dates, discard day, hour, minute and second details
#' time_collapse(FB, period = "monthly", normalize = TRUE)
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
time_collapse <- function(.data, period = "yearly", start_date = NULL,
                          as_sep_col = FALSE, normalize=FALSE, ...) {
  UseMethod("time_collapse")
}

#' @export
time_collapse.default <- function(.data, period = "yearly", start_date = NULL,
                                  as_sep_col = FALSE, normalize=FALSE, ...) {
  stop("Object is not of class `tbl_time`.", call. = FALSE)
}

#' @export
time_collapse.tbl_time <- function(.data, period = "yearly", start_date = NULL,
                                   as_sep_col = FALSE, normalize=FALSE, ...) {

  # Setup
  index_char     <- retrieve_index(.data, as_name = TRUE)
  index_sym      <- rlang::sym(index_char)

  # Check and normalize group period
  period_list <- split_period(period)

  # Keep the original dates as .date if requested
  if(as_sep_col) {
    .data <- dplyr::mutate(.data, !! rlang::sym(".date") := !! index_sym)

    # Order columns correctly
    # tibble::add_column doesn't work because not generic. Strips class.
    index_pos <- which(colnames(.data) == index_char)
    .data <- .data[,c(1:index_pos, ncol(.data), (index_pos+1):(ncol(.data)-1))]
  }

  collapsed_data <- .data %>%

    # Add time groups
    mutate(.time_group = time_group(!! index_sym,
                                    period = period,
                                    start_date = start_date,
                                    ...)) %>%

    # Group by them
    group_by(.data$.time_group, add = TRUE) %>%

    # Normalize or Max the date per group
    mutate(!! index_sym := max(!! index_sym)) %>%

    # Ungroup
    ungroup() %>%

    # Remove .time_group
    dplyr::select(- .data$.time_group)


  if(normalize){
    collapsed_data <- collapsed_data %>%
      mutate(!! index_sym := lubridate::floor_date(!! index_sym, period_list[["period"]]))
  }

  collapsed_data

}

#' @export
time_collapse.grouped_tbl_time <- function(.data,
                                           period = "yearly",
                                           start_date = NULL,
                                           as_sep_col = FALSE,
                                           normalize=FALSE,
                                           ...) {

  time_collapse.tbl_time(.data, period = period, as_sep_col = as_sep_col,
                         start_date = start_date, normalize = normalize) %>%
    group_by(!!! dplyr::groups(.data))

}
