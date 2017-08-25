#' Retrieve a `tbl_time` index
#'
#' Return the index column as a tibble or as a character
#'
#' @param x A `tbl_time` object
#' @param as_name Logical specifying whether to return the the index as a tibble or character
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
    group_syms <- rlang::sym(attr(x, "vars"))
    dplyr::select(x, !!! list(group_syms, attr(x, "index")))
  }
}

#' Expand a `tbl_time` index
#'
#' Retrieve an expanded version of a `tbl_time` index.
#'
#' @details
#'
#' The expanded version of the index contains columns for:
#' * `year`
#' * `month`
#' * `day`
#' * `hour`
#' * `minute`
#' * `second`
#'
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
#' expand_index(x)
#'
expand_index <- function(x) {

  index      <- retrieve_index(x)
  index_name <- rlang::sym(retrieve_index(x, as_name = TRUE))

  dplyr::mutate(index,
         year   = lubridate::year(!! index_name),
         month  = lubridate::month(!! index_name),
         day    = lubridate::day(!! index_name),
         hour   = lubridate::hour(!! index_name),
         minute = lubridate::minute(!! index_name),
         second = lubridate::second(!! index_name)
         )
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
