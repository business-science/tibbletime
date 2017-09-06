#' @export
#' @importFrom tidyr nest
#'
nest.tbl_time <- function(data, ..., .key = "data")  {
  .key <- rlang::enquo(.key)
  tidyverse_execute(data, nest, ..., .key = !! .key)
}

#' @export
tidyr::nest
