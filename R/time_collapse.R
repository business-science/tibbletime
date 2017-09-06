#' Collapse a `tbl_time` object so that all observations in a period share the
#' same date
#'
#' When `time_collapse` is used, the index of a `tbl_time` object is altered
#' so that all dates that fall in a period share a common date.
#'
#' @param .data A `tbl_time` object.
#' @param period A period to collapse to.
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
#' @note
#'
#' The following periods are available:
#' * `"yearly"`
#' * `"quarterly"`
#' * `"monthly"`
#' * `"weekly"`
#' * `"daily"`
#' * `"hour"`
#' * `"minute"`
#' * `"second"`
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
#' # Collapse to weekly dates, but keep the original too
#' time_collapse(FB, period = "weekly", as_sep_col = TRUE)
#'
#' # Grouped functionality -----------------------------------------------------
#'
#' data(FANG)
#' FANG <- FANG %>%
#'   as_tbl_time(date) %>%
#'   group_by(symbol)
#'
#' # Collapse each group to monthly
#' FANG %>%
#'   time_collapse("monthly")
#'
#'
#' @export
time_collapse <- function(.data, period = "yearly",
                          as_sep_col = FALSE, ...) {
  UseMethod("time_collapse")
}

#' @export
time_collapse.default <- function(.data, period = "yearly",
                                  as_sep_col = FALSE, ...) {
  stop("Object is not of class `tbl_time`.", call. = FALSE)
}

#' @export
time_collapse.tbl_time <- function(.data, period = "yearly",
                                   as_sep_col = FALSE, ...) {
  join_cols <- retrieve_index(.data, as_name = TRUE)
  collapse_it(.data, period = period, join_cols = join_cols,
              as_sep_col = as_sep_col, ...)
}

#' @export
time_collapse.grouped_tbl_time <- function(.data, period = "yearly",
                                           as_sep_col = FALSE, ...) {
  join_cols <- c(dplyr::group_vars(.data),
                 retrieve_index(.data, as_name = TRUE))
  collapse_it(.data, period = period, join_cols = join_cols,
              as_sep_col = as_sep_col, ...)
}

# Utils -----

collapse_it <- function(.data, period = "yearly", join_cols,
                        as_sep_col = FALSE, ...) {

  index          <- retrieve_index(.data)
  index_char     <- retrieve_index(.data, as_name = TRUE)
  index_sym      <- rlang::sym(index_char)
  exp_index      <- expand_index(.data, period)

  # Need join_cols = combo of groups and index
  period_cols    <- setdiff(x = colnames(exp_index), y = join_cols)

  # Keep the original dates as .date if requested
  if(as_sep_col) {
    .data <- dplyr::mutate(.data, !! rlang::sym(".date") := !! index_sym)

    # Order columns correctly
    # tibble::add_column doesn't work because not generic. Strips class.
    index_pos <- which(colnames(.data) == index_char)
    .data <- .data[,c(1:index_pos, ncol(.data), (index_pos+1):(ncol(.data)-1))]
  }

  # Join .data with expanded index
  dplyr::left_join(.data, exp_index, by = join_cols) %>%

    # Additionally group by expanded index periods
    dplyr::group_by(!!! rlang::syms(period_cols), add = TRUE) %>%

    # Index column becomes the max of that group
    # Keep the original dates as .date if requested
    dplyr::mutate(!! index_sym := max(!! index_sym)) %>%

    # Regroup by original groups (removes period grouping)
    dplyr::group_by(!!! dplyr::groups(.data)) %>%

    # Remove period cols
    dplyr::select(- dplyr::one_of(period_cols))
}
