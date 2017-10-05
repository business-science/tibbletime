#' @export
#' @importFrom tidyr spread
#'
spread.tbl_time <- function(data, key, value, fill = NA,
                            convert = FALSE, drop = TRUE,
                            sep = NULL)  {

  key   <- rlang::enquo(key)
  value <- rlang::enquo(value)

  tidyverse_execute(data, spread,
                    key = !! key, value = !! value, fill = NA,
                    convert = convert, drop = drop,
                    sep = sep)
}

