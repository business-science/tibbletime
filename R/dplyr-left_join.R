#' @export
#' @importFrom dplyr left_join
#'
left_join.tbl_time <- function(x, y, by = NULL, copy = FALSE, suffix = c(".x", ".y"), ...) {
  dplyr_execute(x, left_join, y = y, by = by, copy = copy, suffix = suffix, ...)
}

#' @export
dplyr::left_join
