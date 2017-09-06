#' Summarise a `tbl_time` by period
#'
#' [time_summarise()] works similarly to [dplyr::summarise()] but with the added
#' benefit of being able to summarise by a time period such as `"yearly"` or
#' `"monthly"`.
#'
#' @inheritParams dplyr::summarise
#' @param .data A `tbl_time` object.
#' @param period A period to summarise by.
#'
#' @details
#'
#' Groups applied using [dplyr::group_by()] are respected.
#'
#' In [dplyr::summarise()], one level of grouping is usually removed.
#' Because an added group for the time index is added in `time_summarise`,
#' none of the original groups are removed.
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
#' @rdname time_summarise
#'
#' @export
#'
time_summarise <- function(.data, period = "yearly", ...) {
  UseMethod("time_summarise")
}

#' @export
#'
time_summarise.tbl_time <- function(.data, period = "yearly", ...) {

  index_sym <- rlang::sym(retrieve_index(.data, as_name = TRUE))

  time_collapse(.data, period) %>%
    dplyr::group_by(!! index_sym, add = TRUE) %>%
    dplyr::summarise(...)
}

#' @export
#'
time_summarise.grouped_tbl_time <- function(.data, period = "yearly", ...) {
  time_summarise.tbl_time(.data, ..., period = period) %>%
    dplyr::group_by(!!! dplyr::groups(.data))
}

# time_summarize ---------------------------------------------------------------

#' @export
#' @rdname time_summarise
time_summarize <- function(.data, period = "yearly", ...) {
  UseMethod("time_summarize")
}

#' @export
#'
time_summarize.tbl_time <- time_summarise.tbl_time

#' @export
#'
time_summarize.grouped_tbl_time <- time_summarise.grouped_tbl_time
