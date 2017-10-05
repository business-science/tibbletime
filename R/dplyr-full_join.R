#' @export
#' @importFrom dplyr full_join
#'
full_join.tbl_time <- function(x, y, by = NULL, copy = FALSE, suffix = c(".x", ".y"), ...) {
  tidyverse_execute(x, full_join, y = y, by = by, copy = copy, suffix = suffix, ...)
}
