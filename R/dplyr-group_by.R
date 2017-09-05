#' @export
#' @importFrom dplyr group_by
#'
group_by.tbl_time <- function(.data, ..., add = FALSE) {
  x <- tidyverse_execute(.data, group_by, ..., add = add)

  # Add a grouped_tbl_time class if not already there
  if("tbl_time" %in% class(x)) {
    class(x) <- unique(c("grouped_tbl_time", class(x)))
  }

  x
}

#' @export
dplyr::group_by

