#' @export
#' @importFrom dplyr filter
#'
filter.tbl_time <- function(.data, ...) {
  tidyverse_execute(.data, filter, ...)
}

# Required to export filter, otherwise:
# Warning: declared S3 method 'filter.tbl_time' not found
# because of stats::filter

#' @export
#'
dplyr::filter
