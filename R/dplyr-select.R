#' @export
#' @importFrom dplyr select
#'
select.tbl_time <- function(.data, ...) {
  tidyverse_execute(.data, select, ...)
}

