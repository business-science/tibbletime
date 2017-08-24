#' @export
#' @importFrom dplyr select
#'
select.tbl_time <- function(.data, ...) {
  tmp_data <- .data
  tmp_data %>%
    declass("tbl_time") %>%
    select(...) %>%
    reclass(.data)
}

#' @export
dplyr::select

