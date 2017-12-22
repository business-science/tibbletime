# Getters for tbl_time objects -------------------------------------------------

#' Getters
#'
#' Accessors to attributes of `tbl_time` objects.
#'
#' @param x A `tbl_time` object.
#'
#' @name getters
#' @export
get_index_quo <- function(x) {
  if(!inherits(x, "tbl_time")) glue_stop("Object is not of class `tbl_time`.")

  index_quo <- attr(x, "index_quo")

  if(is.null(index_quo)) {
    glue_stop("Attribute, `index_quo`, has been lost, ",
              "but class is still `tbl_time`. This should not happen unless ",
              "something has gone horribly wrong.")
  }

  index_quo
}

#' @rdname getters
#' @export
get_index_char <- function(x) {
  rlang::quo_name(get_index_quo(x))
}

#' @rdname getters
#' @export
get_index_col <- function(x) {
  x[[get_index_char(x)]]
}

#' @rdname getters
#' @export
get_index_time_zone <- function(x) {
  if(!inherits(x, "tbl_time")) glue_stop("Object is not of class `tbl_time`.")

  index_time_zone <- attr(x, "index_time_zone")

  if(is.null(index_time_zone)) {
    glue_stop("Attribute, `index_time_zone`, has been lost, ",
              "but class is still `tbl_time`. This should not happen unless ",
              "something has gone horribly wrong.")
  }

  index_time_zone
}

#' @rdname getters
#' @export
get_index_class <- function(x) {
  class(get_index_col(x))[[1]]
}

get_.index_col <- function(x) {
  to_posixct_numeric(get_index_col(x))
}

get_index_dispatcher <- function(x) {
  make_dummy_dispatch_obj(get_index_class(x))
}

# Getters in tbl_time object creation ------------------------------------------

# Get the default time zone. Use a non daylight savings default
# to avoid issues like issue #31
get_default_time_zone <- function() {
  "UTC"
}

get_index_col_time_zone <- function(x) {
  attr(x, "tzone") %||% get_default_time_zone()
}

get_index_col_class <- function(x) {
  class(x)[[1]]
}
