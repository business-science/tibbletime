# New tbl_time creation --------------------------------------------------------
# Currently for internal use, may later be exported when we have more
# packages


#' Create a new tbl_time object
#'
#' Often used internally by developers extending tibbletime
#'
#' @param x A tibble or data.frame
#' @param index_quo The quo that references the index column
#' @param index_time_zone The index time zone
#' @param ... Other attributes passed through to new_tibble()
#' @param subclass A subclass to have as a child
#'
#' @export
#'
new_tbl_time <- function(x, index_quo, index_time_zone, ..., subclass = NULL) {

  stopifnot(is.data.frame(x))
  stopifnot(rlang::is_quosure(index_quo))
  stopifnot(is.character(index_time_zone))

  # Subclass checks, takes care of grouped_tbl_time creation
  subclass <- c(subclass, "tbl_time")
  if("grouped_tbl_time" %in% subclass) {
    subclass <- c(subclass, "grouped_df")
  }

  tibble::new_tibble(
    x,
    index_quo       = index_quo,
    index_time_zone = index_time_zone,
    ...,
    nrow            = nrow(x),
    class           = subclass
  )

}

