#' @export
#' @importFrom dplyr distinct
#'
distinct.tbl_time <- function(.data, ..., .keep_all = FALSE) {
  tmp_data <- .data
  tmp_data %>%
    declass("tbl_time") %>%
    distinct(..., .keep_all = .keep_all) %>%
    reclass(.data)
}

#' @export
dplyr::distinct

#' @importFrom dplyr transmute
#' @export
dplyr::transmute
