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
      index = get_index_quo(old),
      index_time_zone = get_index_time_zone(old),
      subclass = subclass
    )
  }

  else {
    tibble::new_tibble(new, subclass = subclass)
  }

}

index_still_exists <- function(new, old) {
  get_index_char(old) %in% colnames(new)
}
