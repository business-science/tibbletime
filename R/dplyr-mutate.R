#' @export
#' @importFrom dplyr mutate
#'
mutate.tbl_time <- function(.data, ...) {
  tmp_data <- .data
  tmp_data %>%
    declass("tbl_time") %>%
    mutate(...) %>%
    reclass(.data)
}

#' @export
dplyr::mutate

#' @importFrom dplyr transmute
#' @export
dplyr::transmute
