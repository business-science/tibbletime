#' @export
#' @importFrom tidyr gather_
#'
gather_.tbl_time <- function(data, key_col, value_col, gather_cols, na.rm = FALSE,
                             convert = FALSE, factor_key = FALSE)  {
  tidyverse_execute(data, gather_,
                    key_col = key_col, value_col = value_col,
                    gather_cols = gather_cols, na.rm = na.rm,
                    convert = convert, factor_key = factor_key)
}

#' @export
tidyr::gather_
