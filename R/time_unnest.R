#' Unnest a `tibble` containing a list-column of `tbl_time` objects
#'
#' When a `tbl_time` object is nested by [tidyr::nest()], the outer object is
#' often a `tibble`, and the nested list-column contains `tbl_time` objects.
#' This allows you to unnest that list-column and retain time information.
#'
#' @param data A `tibble` containing a list-column of `tbl_time` objects.
#' @param ... Passed on to [tidyr::unnest()].
#'
#' @examples
#'
#' data(FANG)
#' FANG <- as_tbl_time(FANG, date) %>%
#'    group_by(symbol)
#'
#' FANG_nested <- tidyr::nest(FANG)
#'
#' # FANG_nested is not a `tbl_time` anymore
#' class(FANG_nested)
#'
#' # The list column contains the `tbl_time` objects because they have the
#' # index.
#' class(FANG_nested$data[[1]])
#'
#' # Unnest and keep time information
#' time_unnest(FANG_nested)
#'
#' @export
#'
time_unnest <- function(data, ...) {
  UseMethod("time_unnest")
}

#' @export
time_unnest.default <- function(data, ...) {
  stop("Object must be a nested tibble", call. = FALSE)
}

#' @export
time_unnest.tbl_df <- function(data, ...) {

  # Find list columns
  quos <- rlang::quos(...)

  if (rlang::is_empty(quos)) {
    list_cols <- names(data)[purrr::map_lgl(data, purrr::is_list)]
    quos <- rlang::syms(list_cols)
  }

  # Grab nested columns
  nested <- dplyr::transmute(dplyr::ungroup(data), !!! quos)

  # Which list columns contain tbl_time objects? Extract the first one.
  which_tbl_time <- which(purrr::map_lgl(nested, ~inherits(.x[[1]],
                                                           "tbl_time")))
  which_tbl_time <- which_tbl_time[1]
  nested_time <- nested[[which_tbl_time]]

  # Extract inner time classes and attributes
  time_classes <- extract_time_classes(nested_time[[1]])
  time_attrs <- extract_time_attrs(nested_time[[1]])

  # Unnest and add the inner time class/attributes to the outer result
  unnest(data, ...) %>%
    retime(time_classes, time_attrs)
}
