#' @export
#' @importFrom dplyr mutate
mutate.tbl_time <- function(.data, ...) {
  sloop::reconstruct(NextMethod(), .data)
}

#' @export
#' @importFrom dplyr transmute
transmute.tbl_time <- function(.data, ...) {
  sloop::reconstruct(NextMethod(), .data)
}

#' @export
#' @importFrom dplyr summarise
summarise.tbl_time <- function(.data, ...) {
  sloop::reconstruct(NextMethod(), .data)
}

#' @export
#' @importFrom dplyr filter
filter.tbl_time <- function(.data, ...) {
  sloop::reconstruct(NextMethod(), .data)
}

#' @export
#' @importFrom dplyr arrange
arrange.tbl_time <- function(.data, ...) {
  sloop::reconstruct(NextMethod(), .data)
}

#' @export
#' @importFrom dplyr distinct
distinct.tbl_time <- function(.data, ..., .keep_all = FALSE) {
  sloop::reconstruct(NextMethod(), .data)
}

#' @export
#' @importFrom dplyr full_join
#'
full_join.tbl_time <- function(x, y, by = NULL, copy = FALSE, suffix = c(".x", ".y"), ...) {
  sloop::reconstruct(NextMethod(), x)
}

#' @export
#' @importFrom dplyr inner_join
#'
inner_join.tbl_time <- function(x, y, by = NULL, copy = FALSE, suffix = c(".x", ".y"), ...) {
  sloop::reconstruct(NextMethod(), x)
}

#' @export
#' @importFrom dplyr left_join
#'
left_join.tbl_time <- function(x, y, by = NULL, copy = FALSE, suffix = c(".x", ".y"), ...) {
  sloop::reconstruct(NextMethod(), x)
}

#' @export
#' @importFrom dplyr right_join
#'
right_join.tbl_time <- function(x, y, by = NULL, copy = FALSE, suffix = c(".x", ".y"), ...) {
  sloop::reconstruct(NextMethod(), x)
}

#' @export
#' @importFrom dplyr anti_join
#'
anti_join.tbl_time <- function(x, y, by = NULL, copy = FALSE, ...) {
  sloop::reconstruct(NextMethod(), x)
}

#' @export
#' @importFrom dplyr semi_join
#'
semi_join.tbl_time <- function(x, y, by = NULL, copy = FALSE, ...) {
  sloop::reconstruct(NextMethod(), x)
}

#' @importFrom dplyr select
#'
select.tbl_time <- function(.data, ...) {
  sloop::reconstruct(NextMethod(), .data)
}

#' @importFrom dplyr slice
#'
slice.tbl_time <- function(.data, ...) {
  sloop::reconstruct(NextMethod(), .data)
}

#' @export
#' @importFrom dplyr group_by
group_by.tbl_time <- function(.data, ..., add = FALSE) {
  sloop::reconstruct(NextMethod(), .data)
}

#' @importFrom dplyr ungroup
#'
ungroup.tbl_time <- function(x, ...) {
  sloop::reconstruct(NextMethod(), x)
}

