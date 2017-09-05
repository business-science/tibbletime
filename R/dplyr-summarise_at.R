#' @export
#' @importFrom dplyr summarise_at
#'
summarise_at.tbl_time <- function(.tbl, .vars, .funs, ..., .cols = NULL) {
  tidyverse_execute(.tbl, summarise_at, .vars = .vars, .funs = .funs, ..., .cols = .cols)
}

#' @export
#' @importFrom dplyr summarize_at
#'
summarize_at.tbl_time <- summarise_at.tbl_time

#' @export
dplyr::summarise_at

#' @export
dplyr::summarize_at
