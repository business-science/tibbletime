#' @export
#' @importFrom tidyr gather
#'
gather.tbl_time <- function(data, key, value, ...,
                            na.rm = FALSE, convert = FALSE,
                            factor_key = FALSE) {
  dplyr_execute(data, gather, key = key, value = value, ...,
                na.rm = na.rm, convert = convert, factor_key = factor_key)
}

#' @export
#' @importFrom tidyr gather_
#'
gather_.tbl_time <- function(data, key_col, value_col, gather_cols, na.rm = FALSE,
                             convert = FALSE, factor_key = FALSE)  {
  dplyr_execute(data, gather_, key_col = key_col, value_col = value_col,
                gather_cols = gather_cols, na.rm = na.rm,
                convert = convert, factor_key = factor_key)
}

#' @export
tidyr::gather

#' @export
tidyr::gather_
