#' Create `tbl_time` objects
#'
#' `tbl_time` objects have a time index that contains information about
#' which column should be used for time based subsetting and other time based
#' manipulation. Otherwise, they function as normal tibbles.
#'
#' @details
#'
#' The information stored about `tbl_time` objects are the `index` and the
#' `time_zone`. These are stored as attributes, with the `index` as a
#' _quosure_ and the `time_zone` as a string.
#'
#' Currently, only the `Date` and `POSIXct` classes are supported to be
#' time indices.
#'
#' @param x An object to be converted to `tbl_time`. This is generally
#' a [tibble::tibble()], or an object that can first be coerced to a `tibble`.
#' @param index The bare column name of the column to be used as the index.
#' @param ... Arguments passed to [tibble::as_tibble()] if coercion is
#' necessary first.
#'
#' @rdname tbl_time
#'
#' @export
#'
#' @examples
#'
#' # Converting a data.frame to a `tbl_time`
#' # Using Date index
#' ex1 <- data.frame(date = Sys.Date(), value = 1)
#' ex1_tbl_time <- as_tbl_time(ex1, date)
#' class(ex1_tbl_time)
#' attributes(ex1_tbl_time)
#'
#' # Converting a tibble to a `tbl_time`
#' # Using POSIXct index
#' ex2 <- tibble::tibble(
#'   time  = as.POSIXct(c("2017-01-01 10:12:01", "2017-01-02 12:12:01")),
#'   value = c(1, 2)
#' )
#' as_tbl_time(ex2, time)
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
  as_tbl_time(tibble::as_tibble(x, ...), !! index)
}

#' @export
as_tbl_time.tbl_df <- function(x, index, ...) {

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

  # Normal conversion plus added class
  x <- as_tbl_time.tbl_df(x, !! index)
  class(x) <- unique(c("grouped_tbl_time", class(x)))

  x
}

# Utils ----

validate_index <- function(x, index) {

  # Does the column exist in x?
  assertthat::assert_that(rlang::quo_name(index) %in% colnames(x))

  # Is the column time based?
  assertthat::assert_that(is_any_date(dplyr::pull(x, !! index)),
                          msg = "Specified `index` is not time based")
}

is_any_date <- function(x) {
  inherits(x, c("Date", "POSIXct", "POSIXt"))
}

