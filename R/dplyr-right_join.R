#' @export
#' @importFrom dplyr right_join
#'
right_join.tbl_time <- function(x, y, by = NULL, copy = FALSE, suffix = c(".x", ".y"), ...) {
  tidyverse_execute(x, right_join, y = y, by = by, copy = copy, suffix = suffix, ...)
}
