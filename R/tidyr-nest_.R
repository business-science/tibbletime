#' @export
#' @importFrom tidyr nest_
#'
nest_.tbl_time <- function(data, key_col, nest_cols = character())  {
  tidyverse_execute(data, nest_, key_col = key_col, nest_cols = nest_cols)
}

#' @export
tidyr::nest_
