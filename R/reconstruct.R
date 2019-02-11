# Implement generic reconstruct() until sloop is on CRAN

#' Reconstruct an S3 class from a template
#'
#' This is an implementation of `sloop::reconstruct()` that users can
#' ignore. Once `sloop` is on CRAN, this function will be removed and that
#' version will be used. It currently must be exported for use in `tidyquant`.
#'
#' @param new Freshly created object
#' @param old Existing object to use as template
#'
#' @export
reconstruct <- function (new, old) {
  UseMethod("reconstruct", old)
}

#' @export
reconstruct.tbl_time <- function(new, old) {

  # Check subclass, if it was/is a grouped_df,
  # it should also be grouped_tbl_time
  subclass <- NULL
  if(inherits(new, "grouped_df")) {
    subclass <- "grouped_tbl_time"
  }

  # If we have an index
  if(index_still_exists(new, old)) {
    new_tbl_time(
      new,
      index_quo = get_index_quo(old),
      index_time_zone = get_index_time_zone(old),
      subclass = subclass
    )
  }

  else {
    tibble::new_tibble(new, nrow = nrow(new), subclass = subclass)
  }

}

index_still_exists <- function(new, old) {
  get_index_char(old) %in% colnames(new)
}
