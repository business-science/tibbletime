#' @export
#' @importFrom dplyr group_by
#'
group_by.tbl_time <- function(.data, ..., add = FALSE) {
  tmp_data <- .data
  tmp_data <- tmp_data %>%
    declass("tbl_time") %>%
    group_by(..., add = add) %>%
    reclass(.data)

  # Add a grouped_tbl_time class if not already there
  if("tbl_time" %in% class(tmp_data)) {
    class(tmp_data) <- unique(c("grouped_tbl_time", class(tmp_data)))
  }

  tmp_data
}

#' @export
dplyr::group_by

