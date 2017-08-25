#' Convert an object into a `tbl_time` object
#'
#' `tbl_time` objects have a time index that contains information about which column should be
#' used for time based subsetting and other time based manipulation. Otherwise, they function
#' as normal tibbles.
#'
#' @param x stuff!!
#' @param index stuff!!
#' @param ... Currently not used
#'
#' @rdname tbl_time
#'
#' @export
#'
#' @examples
#'
#' examples!!
#'
tbl_time <- function(x, index, ...) {
  index <- rlang::enquo(index)
  as_tbl_time(x, !! index, ...)
}


#' @export
#' @rdname tbl_time
as_tbl_time <- function(x, index, ...) {
  UseMethod("as_tbl_time")
}

#' @export
as_tbl_time.default <- function(x, index, ...) {
  index <- rlang::enquo(index)
  as_tbl_time(tibble::as_tibble(x), !! index, ...)
}


#' @export
as_tbl_time.data.frame <- function(x, index, ...) {

  # Capture index
  index <- rlang::enquo(index)

  # Enforce use of index
  assertthat::assert_that(!rlang::quo_is_missing(index),
                          msg = "Please include a bare column name for the `index`")

  # Validate index
  validate_index(x, index)

  # Set main class and time attributes
  class(x) <- unique(c("tbl_time", class(x)))
  attr(x, "index") <- index
  time_zone <- attr(dplyr::pull(x, !! index), "tzone")
  attr(x, "time_zone") <- ifelse(is.null(time_zone), Sys.timezone(), time_zone)

  x
}

#' @export
as_tbl_time.grouped_df <- function(x, index, ...) {

  # Capture index
  index <- rlang::enquo(index)

  x <- as_tbl_time.data.frame(x, !! index)
  class(x) <- unique(c("grouped_tbl_time", class(x)))

  x
}


# Utils ----

validate_index <- function(x, index) {

  is_any_date <- function(x) {
    inherits(x, c("Date", "POSIXct", "POSIXt"))
  }

  # Does the column exist in x?
  assertthat::assert_that(rlang::quo_name(index) %in% colnames(x))

  # Is the column time based?
  assertthat::assert_that(is_any_date(dplyr::pull(x, !! index)),
                          msg = "Specified `index` is not time based")
}

