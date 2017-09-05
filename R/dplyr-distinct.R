#' @export
#' @importFrom dplyr distinct
#'
distinct.tbl_time <- function(.data, ..., .keep_all = FALSE) {
  dplyr_execute(.data, distinct, ..., .keep_all = .keep_all)
}

#' @export
dplyr::distinct
