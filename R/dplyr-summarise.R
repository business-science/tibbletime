#' @export
#' @importFrom dplyr summarise
#'
summarise.tbl_time <- function(.data, ...) {
  tidyverse_execute(.data, summarise, ...)
}

#' @export
#' @importFrom dplyr summarize
summarize.tbl_time <- summarise.tbl_time

#' @export
dplyr::summarise

#' @export
dplyr::summarize
