#' @export
#' @importFrom dplyr filter
#'
filter.tbl_time <- function(.data, ...) {
  tidyverse_execute(.data, filter, ...)
}

#' @export
dplyr::filter
