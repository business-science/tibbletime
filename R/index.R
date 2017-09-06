#' Retrieve a `tbl_time` index
#'
#' Return the index column as a tibble or as a character
#'
#' @param x A `tbl_time` object.
#' @param as_name Logical specifying whether to return
#' the index as a tibble or a character of the column name.
#'
#' @export
#'
#' @examples
#'
#' x <- tibble::tibble(
#'   date  = as.Date(c('2017-01-01', '2017-01-02')),
#'   value = c(1, 2)
#' )
#'
#' x <- as_tbl_time(x, date)
#'
#' retrieve_index(x)
#'
retrieve_index <- function(x, as_name = FALSE) {
  UseMethod("retrieve_index")
}

#' @export
retrieve_index.default <- function(x, as_name = FALSE) {
  stop("Object is not of class `tbl_time`, have you called `as_tbl_time()`?", call. = FALSE)
}

#' @export
retrieve_index.tbl_time <- function(x, as_name = FALSE) {

  assertthat::assert_that(rlang::quo_name(attr(x, "index")) %in% colnames(x),
                          msg = "`index` has been removed from the time tibble")

  if(as_name) {
    rlang::quo_name(attr(x, "index"))
  } else {
    dplyr::select(x, !! attr(x, "index"))
  }
}

#' @export
retrieve_index.grouped_tbl_time <- function(x, as_name = FALSE) {

  assertthat::assert_that(rlang::quo_name(attr(x, "index")) %in% colnames(x),
                          msg = "`index` has been removed from the time tibble")

  if(as_name) {
    rlang::quo_name(attr(x, "index"))
  } else {
    group_syms <- rlang::syms(attr(x, "vars"))
    dplyr::select(x, !!! c(group_syms, attr(x, "index")))
  }
}

#' Expand a `tbl_time` index
#'
#' Retrieve an expanded version of a `tbl_time` index.
#'
#' @details
#'
#' The expanded version of the index only expands as far as necessary
#' to generate a unique row for the period selected
#'
#'
#' @param x A `tbl_time` object
#' @param period The most granular period to expand to
#'
#' @export
#'
#' @examples
#'
#' x <- tibble::tibble(
#'   date  = as.Date(c('2017-01-01', '2017-01-02')),
#'   value = c(1, 2)
#' )
#'
#' x <- as_tbl_time(x, date)
#'
#' expand_index(x, "monthly")
#'
expand_index <- function(x, period) {

  # Index tibble and symbol index name
  index      <- retrieve_index(x)
  index_name <- rlang::sym(retrieve_index(x, as_name = TRUE))

  # Only as much expansion as necessary
  period_list <- period_to_syms(period)

  # Construct the list of do.call()'s
  call_list <- purrr::map(.x = period_list,
                          .f = function(.x) {
                            period_fun <- get(as.character(.x), asNamespace("lubridate")) %>%
                              rlang::as_quosure()
                            rlang::expr(do.call(!! period_fun, args = list(!! index_name)))
                            }
                          )

  # Add names. These become column names
  names(call_list) <- as.character(period_list)

  dplyr::mutate(index, !!! call_list)
}

#' Retrieve a `tbl_time` time zone
#'
#' Returns the `time_zone` attribute as a character
#'
#' @param x A `tbl_time` object
#'
#' @export
#'
#' @examples
#'
#' x <- tibble::tibble(
#'   date  = as.Date(c('2017-01-01', '2017-01-02')),
#'   value = c(1, 2)
#' )
#'
#' x <- as_tbl_time(x, date)
#'
#' retrieve_time_zone(x)
#'
retrieve_time_zone <- function(x) {
  UseMethod("retrieve_time_zone")
}

#' @export
retrieve_time_zone.tbl_time <- function(x) {
  attr(x, "time_zone")
}
