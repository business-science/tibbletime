#' @export
#' @importFrom dplyr slice
#'
slice.tbl_time <- function(.data, ...) {
  tidyverse_execute(.data, slice, ...)
}
