#' @export
#' @importFrom tidyr unnest
#'
unnest.tbl_time <- function(data, ..., .drop = NA, .id = NULL, .sep = NULL)  {
  tidyverse_execute(data, unnest, ..., .drop = .drop, .id = .id, .sep = .sep)
}

#' @export
tidyr::unnest
