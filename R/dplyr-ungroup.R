#' @export
#' @importFrom dplyr ungroup
#'
ungroup.tbl_time <- function(x, ...) {
  tmp_data <- x
  tmp_data <- tmp_data %>%
    declass("tbl_time") %>%
    ungroup(...) %>%
    reclass(x)

  class(tmp_data) <- setdiff(class(tmp_data), c("grouped_df", "grouped_tbl_time"))

  tmp_data
}

#' @export
dplyr::ungroup

