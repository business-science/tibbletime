#' Retrieve a `tbl_time` index
#'
#' Returns the index column as a tibble
#'
#'
#' @param x stuff!!
#' @param as_name stuff!!
#'
#' @export
#'
#' @examples
#'
#' examples!!
#'
retrieve_index <- function(x, as_name = FALSE) {
  UseMethod("retrieve_index")
}

#' @export
retrieve_index.tbl_time <- function(x, as_name = FALSE) {

  if(as_name) {
    rlang::quo_name(attr(x, "index"))
  } else {
    dplyr::select(x, !! attr(x, "index"))
  }
}

#' @export
retrieve_index.grouped_tbl_time <- function(x, as_name = FALSE) {

  if(as_name) {
    rlang::quo_name(attr(x, "index"))
  } else {
    group_syms <- rlang::sym(attr(x, "vars"))
    dplyr::select(x, !!! list(group_syms, attr(x, "index")))
  }
}

#' Expand a `tbl_time` index into it's time signature
#'
#' Returns the index column as a tibble
#'
#'
#' @param x stuff!!
#' @param as_name stuff!!
#'
#' @export
#'
#' @examples
#'
#' examples!!
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
#' Returns the time zone attribute as a character
#'
#'
#' @param x stuff!!
#' @param as_name stuff!!
#'
#' @export
#'
#' @examples
#'
#' examples!!
#'
retrieve_time_zone <- function(x) {
  UseMethod("retrieve_time_zone")
}

#' @export
retrieve_time_zone.tbl_time <- function(x) {
  attr(x, "time_zone")
}
