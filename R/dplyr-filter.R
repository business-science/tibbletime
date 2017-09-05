#' @export
#' @importFrom dplyr filter
#'
filter.tbl_time <- function(.data, ...) {
  dplyr_execute(.data, filter, ...)
}

#' @export
dplyr::filter
