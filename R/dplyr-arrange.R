#' @export
#' @importFrom dplyr arrange
#'
arrange.tbl_time <- function(.data, ...) {
  tidyverse_execute(.data, arrange, ...)
}

#' @export
#' @importFrom dplyr arrange
#'
arrange.grouped_tbl_time <- function(.data, ..., .by_group = FALSE) {
  tidyverse_execute(.data, arrange, ..., .by_group = .by_group)
}

#' @export
dplyr::arrange
