#' @export
#' @importFrom dplyr mutate
mutate.tbl_time <- function(.data, ...) {
  #reconstruct(NextMethod(), .data)
  copy_.data <- new_tbl_time(.data, get_index_quo(.data), get_index_time_zone(.data))
  reconstruct(NextMethod(), copy_.data)
}

#' @export
#' @importFrom dplyr transmute
transmute.tbl_time <- function(.data, ...) {
  #reconstruct(NextMethod(), .data)
  copy_.data <- new_tbl_time(.data, get_index_quo(.data), get_index_time_zone(.data))
  reconstruct(NextMethod(), copy_.data)
}

#' @export
#' @importFrom dplyr summarise
summarise.tbl_time <- function(.data, ...) {
  #reconstruct(NextMethod(), .data)
  copy_.data <- new_tbl_time(.data, get_index_quo(.data), get_index_time_zone(.data))
  reconstruct(NextMethod(), copy_.data)
}

#' @export
#' @importFrom dplyr filter
filter.tbl_time <- function(.data, ...) {
  #reconstruct(NextMethod(), .data)
  copy_.data <- new_tbl_time(.data, get_index_quo(.data), get_index_time_zone(.data))
  reconstruct(NextMethod(), copy_.data)
}

# Required to export filter, otherwise:
# Warning: declared S3 method 'filter.tbl_time' not found
# because of stats::filter

#' @export
#'
dplyr::filter

#' @export
#' @importFrom dplyr arrange
arrange.tbl_time <- function(.data, ...) {
  #reconstruct(NextMethod(), .data)
  copy_.data <- new_tbl_time(.data, get_index_quo(.data), get_index_time_zone(.data))
  reconstruct(NextMethod(), copy_.data)
}

#' @export
#' @importFrom dplyr distinct
distinct.tbl_time <- function(.data, ..., .keep_all = FALSE) {
  #reconstruct(NextMethod(), .data)
  copy_.data <- new_tbl_time(.data, get_index_quo(.data), get_index_time_zone(.data))
  reconstruct(NextMethod(), copy_.data)
}

#' @export
#' @importFrom dplyr full_join
#'
full_join.tbl_time <- function(x, y, by = NULL, copy = FALSE, suffix = c(".x", ".y"), ...) {
  #reconstruct(NextMethod(), x)
  copy_.data <- new_tbl_time(x, get_index_quo(x), get_index_time_zone(x))
  reconstruct(NextMethod(), copy_.data)
}

#' @export
#' @importFrom dplyr inner_join
#'
inner_join.tbl_time <- function(x, y, by = NULL, copy = FALSE, suffix = c(".x", ".y"), ...) {
  #reconstruct(NextMethod(), x)
  copy_.data <- new_tbl_time(x, get_index_quo(x), get_index_time_zone(x))
  reconstruct(NextMethod(), copy_.data)
}

#' @export
#' @importFrom dplyr left_join
#'
left_join.tbl_time <- function(x, y, by = NULL, copy = FALSE, suffix = c(".x", ".y"), ...) {
  #reconstruct(NextMethod(), x)
  copy_.data <- new_tbl_time(x, get_index_quo(x), get_index_time_zone(x))
  reconstruct(NextMethod(), copy_.data)
}

#' @export
#' @importFrom dplyr right_join
#'
right_join.tbl_time <- function(x, y, by = NULL, copy = FALSE, suffix = c(".x", ".y"), ...) {
  #reconstruct(NextMethod(), x)
  copy_.data <- new_tbl_time(x, get_index_quo(x), get_index_time_zone(x))
  reconstruct(NextMethod(), copy_.data)
}

#' @export
#' @importFrom dplyr anti_join
#'
anti_join.tbl_time <- function(x, y, by = NULL, copy = FALSE, ...) {
  #reconstruct(NextMethod(), x)
  copy_.data <- new_tbl_time(x, get_index_quo(x), get_index_time_zone(x))
  reconstruct(NextMethod(), copy_.data)
}

#' @export
#' @importFrom dplyr semi_join
#'
semi_join.tbl_time <- function(x, y, by = NULL, copy = FALSE, ...) {
  #reconstruct(NextMethod(), x)
  copy_.data <- new_tbl_time(x, get_index_quo(x), get_index_time_zone(x))
  reconstruct(NextMethod(), copy_.data)
}

#' @export
#' @importFrom dplyr select
#'
select.tbl_time <- function(.data, ...) {
  #reconstruct(NextMethod(), .data)
  copy_.data <- new_tbl_time(.data, get_index_quo(.data), get_index_time_zone(.data))
  reconstruct(NextMethod(), copy_.data)
}

#' @export
#' @importFrom dplyr slice
#'
slice.tbl_time <- function(.data, ...) {
  #reconstruct(NextMethod(), .data)
  copy_.data <- new_tbl_time(.data, get_index_quo(.data), get_index_time_zone(.data))
  reconstruct(NextMethod(), copy_.data)
}

#' @export
#' @importFrom dplyr group_by
group_by.tbl_time <- function(.data, ..., add = FALSE) {
  #reconstruct(NextMethod(), .data)
  copy_.data <- new_tbl_time(.data, get_index_quo(.data), get_index_time_zone(.data))
  reconstruct(NextMethod(), copy_.data)
}

#' @export
#' @importFrom dplyr ungroup
#'
ungroup.tbl_time <- function(x, ...) {
  #reconstruct(NextMethod(), x)
  copy_.data <- new_tbl_time(x, get_index_quo(x), get_index_time_zone(x))
  reconstruct(NextMethod(), copy_.data)
}

#' @export
#' @importFrom dplyr rename
#'
rename.tbl_time <- function(.data, ...) {
  # rename() is supported in the case where the user does `date2 = date`.
  # The result should be a new tbl_time with a new index corresponding
  # to the renaming. See #56.

  #reconstruct(NextMethod(), .data)
  copy_.data <- new_tbl_time(.data, get_index_quo(.data), get_index_time_zone(.data))
  out <- reconstruct(NextMethod(), copy_.data)

  # Catch renamed index (if applicable) and create a new tbl_time using it
  dots <- rlang::enquos(...)
  dots_named <- purrr::map(dots, rlang::quo_name)

  # Check if any RHS of the dots match the name of the original index
  new_index_lgl <- purrr::map_lgl(dots_named, ~.x == get_index_char(.data))

  if(any(new_index_lgl)) {

    # Take only the dots that match the original index
    new_index <- new_index_lgl[new_index_lgl]

    # If renamed more than once, take the last (same as dplyr::rename)
    if(length(new_index) > 1) {
      new_index <- new_index[length(new_index)]
    }

    index_expr <- as.name(names(new_index))
    index_quo <- rlang::new_quosure(index_expr)

    # New tbl_time, with new index but same time zone
    out <- new_tbl_time(out, index_quo, get_index_time_zone(.data))
  }

  out
}

### Backwards compat support for deprecated standard eval dplyr

# Only a few of them need it. arrange_.tbl_df() directly calls arrange_impl()
# causing a problem.

#' @export
#' @importFrom dplyr arrange_
#'
arrange_.tbl_time <- function(.data, ..., .dots = list()) {
  #reconstruct(NextMethod(), .data)
  copy_.data <- new_tbl_time(.data, get_index_quo(.data), get_index_time_zone(.data))
  reconstruct(NextMethod(), copy_.data)
}

#' @export
#' @importFrom dplyr mutate_
#'
mutate_.tbl_time <- function(.data, ..., .dots = list()) {
  #reconstruct(NextMethod(), .data)
  copy_.data <- new_tbl_time(.data, get_index_quo(.data), get_index_time_zone(.data))
  reconstruct(NextMethod(), copy_.data)
}

#' @export
#' @importFrom dplyr summarise_
#'
summarise_.tbl_time <- function(.data, ..., .dots = list()) {
  #reconstruct(NextMethod(), .data)
  copy_.data <- new_tbl_time(.data, get_index_quo(.data), get_index_time_zone(.data))
  reconstruct(NextMethod(), copy_.data)
}

#' @export
#' @importFrom dplyr summarize_
#'
summarize_.tbl_time <- function(.data, ..., .dots = list()) {
  #reconstruct(NextMethod(), .data)
  copy_.data <- new_tbl_time(.data, get_index_quo(.data), get_index_time_zone(.data))
  reconstruct(NextMethod(), copy_.data)
}

#' @export
#' @importFrom dplyr slice_
#'
slice_.tbl_time <- function(.data, ..., .dots = list()) {
  #reconstruct(NextMethod(), .data)
  copy_.data <- new_tbl_time(.data, get_index_quo(.data), get_index_time_zone(.data))
  reconstruct(NextMethod(), copy_.data)
}
