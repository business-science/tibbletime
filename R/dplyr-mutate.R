#' @export
#' @importFrom dplyr mutate
#'
mutate.tbl_time <- function(.data, ...) {
  tidyverse_execute(.data, fun = mutate, ...)
}

#' @export
#' @importFrom dplyr transmute
#'
transmute.tbl_time <- function(.data, ...) {
  tidyverse_execute(.data, fun = transmute, ...)
}
