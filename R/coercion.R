#' Create `tbl_time` objects
#'
#' `tbl_time` objects have a time index that contains information about
#' which column should be used for time-based subsetting and other time-based
#' manipulation. Otherwise, they function as normal tibbles.
#'
#' @details
#'
#' The information stored about `tbl_time` objects are the `index_quo` and the
#' `index_time_zone`. These are stored as attributes, with the `index_quo` as a
#' [rlang::quosure()] and the `time_zone` as a string.
#'
#' Currently, `Date` and `POSIXct` classes are fully supported. `yearmon`,
#' `yearqtr`, and `hms` have experimental support. Due to dplyr's
#' handling of S3 classes like these 3, the classes are lost when you
#' manipulate the index columns directly.
#'
#' @param x An object to be converted to `tbl_time`. This is generally
#' a [tibble::tibble()], or an object that can first be coerced to a `tibble`.
#' @param index The bare column name of the column to be used as the index.
#' @param ... Arguments passed to [tibble::as_tibble()] if coercion is
#' necessary first.
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
#' @export
#' @rdname tbl_time
as_tbl_time <- function(x, index = NULL, ...) {
  UseMethod("as_tbl_time")
}

#' @export
as_tbl_time.default <- function(x, index = NULL, ...) {
  index_quo <- rlang::enquo(index)

  # Default to as_tibble for any error handling
  # If it can be converted to tibble, then try and convert to tbl_time
  as_tbl_time(tibble::as_tibble(x, ...), !! index_quo)
}

#' @export
as_tbl_time.tbl_df <- function(x, index = NULL, ...) {
  index_quo <- rlang::enquo(index)

  # Pass off to helper
  tbl_time(x, !! index_quo)
}

# Parent coercion --------------------------------------------------------------

#' @export
#' @importFrom tibble as_tibble
as_tibble.tbl_time <- function(x, ...) {

  # Remove index_* attributes
  for(attrib in index_attributes()) {
    attr(x, attrib) <- NULL
  }

  tibble::new_tibble(x, ...)
}

#' @export
#' @importFrom tibble as_tibble
as_tibble.grouped_tbl_time <- function(x, ...) {

  # Remove index_* attributes
  for(attrib in index_attributes()) {
    attr(x, attrib) <- NULL
  }

  tibble::new_tibble(x, ..., subclass = "grouped_df")
}

