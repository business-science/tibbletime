# Getters for tbl_time objects -------------------------------------------------

#' Getters
#'
#' Accessors to attributes of `tbl_time` objects.
#'
#' @param .tbl_time A `tbl_time` object.
#'
#' @name getters
#' @export
get_index_quo <- function(.tbl_time) {
  if(!inherits(.tbl_time, "tbl_time")) glue_stop("Object is not of class `tbl_time`.")

  index_quo <- attr(.tbl_time, "index_quo")

  if(is.null(index_quo)) {
    glue_stop("Attribute, `index_quo`, has been lost, ",
              "but class is still `tbl_time`. This should not happen unless ",
              "something has gone horribly wrong.")
  }

  index_quo
}

#' @rdname getters
#' @export
get_index_char <- function(.tbl_time) {
  rlang::quo_name(get_index_quo(.tbl_time))
}

#' @rdname getters
#' @export
get_index_col <- function(.tbl_time) {
  .tbl_time[[get_index_char(.tbl_time)]]
}

#' @rdname getters
#' @export
get_index_time_zone <- function(.tbl_time) {
  if(!inherits(.tbl_time, "tbl_time")) glue_stop("Object is not of class `tbl_time`.")

  index_time_zone <- attr(.tbl_time, "index_time_zone")

  if(is.null(index_time_zone)) {
    glue_stop("Attribute, `index_time_zone`, has been lost, ",
              "but class is still `tbl_time`. This should not happen unless ",
              "something has gone horribly wrong.")
  }

  index_time_zone
}

#' @rdname getters
#' @export
get_index_class <- function(.tbl_time) {
  class(get_index_col(.tbl_time))[[1]]
}

get_.index_col <- function(.tbl_time) {
  to_posixct_numeric(get_index_col(.tbl_time))
}

get_index_dispatcher <- function(.tbl_time) {
  make_dummy_dispatch_obj(get_index_class(.tbl_time))
}

# Getters in tbl_time object creation ------------------------------------------

# Get the default time zone. Use a non daylight savings default
# to avoid issues like issue #31
get_default_time_zone <- function() {
  "UTC"
}

get_index_col_time_zone <- function(.tbl_time) {
  attr(.tbl_time, "tzone") %||% get_default_time_zone()
}

get_index_col_class <- function(.tbl_time) {
  class(.tbl_time)[[1]]
}
