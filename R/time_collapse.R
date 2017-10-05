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
time_collapse <- function(.data, period = "yearly", start_date = NULL,
                          as_sep_col = FALSE, ...) {
  UseMethod("time_collapse")
}

#' @export
time_collapse.default <- function(.data, period = "yearly", start_date = NULL,
                                  as_sep_col = FALSE, ...) {
  stop("Object is not of class `tbl_time`.", call. = FALSE)
}

#' @export
time_collapse.tbl_time <- function(.data, period = "yearly", start_date = NULL,
                                   as_sep_col = FALSE, ...) {

  # Setup
  index_char     <- retrieve_index(.data, as_name = TRUE)
  index_sym      <- rlang::sym(index_char)

  # Keep the original dates as .date if requested
  if(as_sep_col) {
    .data <- dplyr::mutate(.data, !! rlang::sym(".date") := !! index_sym)

    # Order columns correctly
    # tibble::add_column doesn't work because not generic. Strips class.
    index_pos <- which(colnames(.data) == index_char)
    .data <- .data[,c(1:index_pos, ncol(.data), (index_pos+1):(ncol(.data)-1))]
  }

  .data %>%

    # Add time groups
    mutate(.time_group = time_group(!! index_sym,
                                    period = period,
                                    start_date = start_date,
                                    ...)) %>%

    # Group by them
    group_by(.data$.time_group, add = TRUE) %>%

    # Max the date per group
    mutate(!! index_sym := max(!! index_sym)) %>%

    # Ungroup
    ungroup() %>%

    # Remove .time_group
    dplyr::select(- .data$.time_group)

}

#' @export
time_collapse.grouped_tbl_time <- function(.data,
                                           period = "yearly",
                                           start_date = NULL,
                                           as_sep_col = FALSE,
                                           ...) {

  time_collapse.tbl_time(.data, period = period, as_sep_col = as_sep_col,
                         start_date = start_date) %>%
    group_by(!!! dplyr::groups(.data))

}
