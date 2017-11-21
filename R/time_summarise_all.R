#' @export
#'
time_summarise_all <- function(.tbl, .funs, period = "yearly",
                           ..., start_date = NULL) {
  UseMethod("time_summarise_all")
}

#' @export
time_summarise_all.default <- function(.tbl, .funs, period = "yearly",
                                   ..., start_date = NULL) {
  stop("Object is not of class `tbl_time`.", call. = FALSE)
}

#' @export
#'
time_summarise_all.tbl_time <- function(.tbl, .funs, period = "yearly",
                                    ..., start_date = NULL) {
  
  index_quo <- get_index_quo(.tbl)
  
  .tbl_summarised <- time_collapse(.tbl, period = period, start_date = start_date) %>%
    dplyr::group_by(!! index_quo, add = TRUE) %>%
    dplyr::summarise_all(.funs = .funs, ...)
  
  sloop::reconstruct(.tbl_summarised, .tbl)
}

#' @export
#'
time_summarise_all.grouped_tbl_time <- function(.tbl, .funs, period = "yearly",
                                            ..., start_date = NULL) {
  .tbl_summarised <- time_summarise_all.tbl_time(.tbl, .funs = .funs, period = period, ...,
                          start_date = start_date) %>%
    dplyr::group_by(!!! dplyr::groups(.tbl))
  
  sloop::reconstruct(.tbl_summarised, .tbl)
}

# time_summarize_all -----------------------------------------------------------

#' @export
#' @rdname time_summarise
time_summarize_all <- function(.tbl, .funs, period = "yearly",
                           ..., start_date = NULL) {
  UseMethod("time_summarize_all")
}

#' @export
#'
time_summarize_all.tbl_time <- time_summarise_all.tbl_time

#' @export
#'
time_summarize_all.grouped_tbl_time <- time_summarise_all.grouped_tbl_time
