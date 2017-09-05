#' @export
#' @importFrom dplyr summarise_at
#'
summarise_at.tbl_time <- function(.tbl, .vars, .funs, ..., .cols = NULL) {
  tidyverse_execute(.tbl, summarise_at, .vars = .vars, .funs = .funs, ..., .cols = .cols)
}

#' @export
dplyr::summarise_at

