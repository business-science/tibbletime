#' @export
#' @importFrom tidyr gather
#'
gather.tbl_time <- function(data, key = "key", value = "value", ..., na.rm = FALSE,
                             convert = FALSE, factor_key = FALSE)  {

  key   <- rlang::enquo(key)
  value <- rlang::enquo(value)

  tidyverse_execute(data, gather,
                    key = !! key, value = !! value, ..., na.rm = na.rm,
                    convert = convert, factor_key = factor_key)
}

