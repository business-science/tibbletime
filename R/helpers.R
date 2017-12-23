#' @export
#' @rdname tbl_time
tbl_time <- function(x, index = NULL) {

  # Capture index
  index_quo <- rlang::enquo(index)

  # Enforce index use
  assert_index_use(index_quo)

  # Index as character
  index_char <- rlang::quo_name(index_quo)

  # Get index column
  index_col <- x[[index_char]]

  # Get time zone
  index_time_zone <- get_index_col_time_zone(index_col)
  
  # Check grouped
  subclass <- NULL
  if(inherits(x, "grouped_df")) {
    subclass <- "grouped_tbl_time"
  }

  # Create and validate
  validate_tbl_time(
    new_tbl_time(
      x,
      index_quo = index_quo,
      index_time_zone = index_time_zone,
      subclass = subclass
    )
  )
}

# Util -------------------------------------------------------------------------

assert_index_use <- function(x) {
  assertthat::assert_that(
    !rlang::quo_is_null(x),
    msg = "Please include a bare column name for the `index`"
  )
}
