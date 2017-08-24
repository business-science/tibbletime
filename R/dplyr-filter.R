#' @export
#' @importFrom dplyr filter
#'
filter.tbl_time <- function(.data, ...) {
  tmp_data <- .data
  tmp_data %>%
    declass("tbl_time") %>%
    filter(...) %>%
    reclass(.data)
}

#' @export
dplyr::filter
