#' @export
#' @importFrom dplyr inner_join
#'
inner_join.tbl_time <- function(x, y, by = NULL, copy = FALSE, suffix = c(".x", ".y"), ...) {
  tidyverse_execute(x, inner_join, y = y, by = by, copy = copy, suffix = suffix, ...)
}

