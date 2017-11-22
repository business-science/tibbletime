#' @export
#'
time_summarise_if <- function(.tbl, .predicate, .funs, period = "yearly",
                           ..., start_date = NULL) {
  UseMethod("time_summarise_if")
}

#' @export
time_summarise_if.default <- function(.tbl, .predicate, .funs, period = "yearly",
                                   ..., start_date = NULL) {
  stop("Object is not of class `tbl_time`.", call. = FALSE)
}

#' @export
#'
time_summarise_if.tbl_time <- function(.tbl, .predicate, .funs, period = "yearly",
                                    ..., start_date = NULL) {

  index_quo <- get_index_quo(.tbl)

  .tbl_summarised <- time_collapse(.tbl, period = period, start_date = start_date) %>%
    dplyr::group_by(!! index_quo, add = TRUE) %>%
    dplyr::summarise_if(.predicate = .predicate, .funs = .funs, ...)

  sloop::reconstruct(.tbl_summarised, .tbl)
}

#' @export
#'
time_summarise_if.grouped_tbl_time <- function(.tbl, .predicate, .funs, period = "yearly",
                                            ..., start_date = NULL) {
  .tbl_summarised <- time_summarise_if.tbl_time(.tbl, .funs = .funs, period = period, ...,
                          start_date = start_date) %>%
    dplyr::group_by(!!! dplyr::groups(.tbl))

  sloop::reconstruct(.tbl_summarised, .tbl)
}

# time_summarize_if -----------------------------------------------------------

#' @export
#' @rdname time_summarise
time_summarize_if <- function(.tbl, .predicate, .funs, period = "yearly",
                           ..., start_date = NULL) {
  UseMethod("time_summarize_if")
}

#' @export
#'
time_summarize_if.tbl_time <- time_summarise_if.tbl_time

#' @export
#'
time_summarize_if.grouped_tbl_time <- time_summarise_if.grouped_tbl_time
