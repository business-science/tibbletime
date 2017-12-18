# Getters for tbl_time objects -------------------------------------------------

#' Getters
#'
#' @name getters
#' @export
get_index_quo <- function(x) {
  attr(x, "index_quo")
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

get_.index_col <- function(x) {
  to_posixct_numeric(get_index_col(x))
}

#' @rdname getters
#' @export
get_index_time_zone <- function(x) {
  attr(x, "index_time_zone")
}

#' @rdname getters
#' @export
get_index_class <- function(x) {
  class(get_index_col(x))[[1]]
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
