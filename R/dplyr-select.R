#' @export
#' @importFrom dplyr select
#'
select.tbl_time <- function(.data, ...) {
  dplyr_execute(.data, select, ...)
}

#' @export
dplyr::select

