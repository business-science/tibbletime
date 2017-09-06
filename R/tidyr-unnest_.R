#' @export
#' @importFrom tidyr unnest_
#'
unnest_.tbl_time <- function(data, unnest_cols, .drop = NA, .id = NULL, .sep = NULL)  {
  tidyverse_execute(data, unnest_, unnest_cols = unnest_cols, .drop = .drop, .id = .id, .sep = .sep)
}

#' @export
tidyr::unnest_
