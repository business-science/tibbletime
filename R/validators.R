# Main validator
validate_tbl_time <- function(x) {
  assert_index_exists_in_colnames(x)
  assert_index_class_is_allowed(x)
  x
}

assert_index_exists_in_colnames <- function(x) {
  index_char <- get_index_char(x)
  assertthat::assert_that(
    index_char %in% colnames(x),
    msg = "Specified `index` is not a column of x"
  )
}

assert_index_class_is_allowed <- function(x) {
  index_char <- get_index_char(x)
  index_col <- x[[index_char]]
  assertthat::assert_that(
    inherits_allowed_datetime(index_col),
    msg = "Specified `index` is not time based"
  )
}
