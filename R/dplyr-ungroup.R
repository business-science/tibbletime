#' @export
#' @importFrom dplyr ungroup
#'
ungroup.tbl_time <- function(x, ...) {
  x <- tidyverse_execute(x, ungroup, ...)
  class(x) <- setdiff(class(x), "grouped_tbl_time")

  x
}

#' @export
dplyr::ungroup

