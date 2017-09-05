#' @export
#' @importFrom dplyr mutate
#'
mutate.tbl_time <- function(.data, ...) {
  dplyr_execute(.data, mutate, ...)
}

#' @export
dplyr::mutate

#' @importFrom dplyr transmute
#' @export
dplyr::transmute
