#' @export
#' @importFrom tidyr nest
#'
nest.tbl_time <- function(data, ..., .key = "data")  {
  .key <- rlang::enquo(.key)
  .key_sym <- rlang::sym(rlang::quo_name(.key))

  # Attributes to keep on the list-column
  # Classes are kept on nest
  time_attrs <- list(
    index     = attr(data, "index"),
    time_zone = attr(data, "time_zone")
  )

  # silent_retime because normally the outer tibble won't have dates anymore
  tidyverse_execute(data, nest, ..., .key = !! .key, silent_retime = TRUE) %>%

    # Each individual element of the list-column
    # should only be a tbl_time (not grouped if it was before)
    mutate(!! .key_sym := purrr::map(!! .key_sym, ~retime(.x,
                                           time_classes = "tbl_time",
                                           time_attrs = time_attrs,
                                           silent_retime = TRUE)))
}

