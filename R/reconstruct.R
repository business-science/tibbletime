reconstruct.tbl_time <- function(new, old) {
  
  # Check subclass
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

# reconstruct.grouped_tbl_time <- function(new, old) {
#   
#   has_index  <- index_still_exists(new, old)
#   has_groups <- dplyr::is.grouped_df(new)
#   has_index_and_groups <- has_index & has_groups
#   
#   # Easy case, still grouped_tbl_time
#   if(has_index_and_groups) {
#     new_grouped_tbl_time(
#       new,
#       index = get_index_quo(old),
#       index_time_zone = get_index_time_zone(old)
#     )
#   } 
#   
#   # If it has index or group, but not both
#   else if(has_index | has_groups) {
#     
#     # tbl_time
#     if(has_index) {
#       new_tbl_time(
#         new, 
#         index_quo = get_index_quo(old), 
#         index_time_zone = get_index_time_zone(old)
#       )
#     } 
#     
#     # grouped_df
#     else if(has_groups) {
#       dplyr::grouped_df(new, dplyr::group_vars(new))
#     }
#   } 
#   
#   # No index, no groups
#   else {
#     as_tibble(new)
#   }
#   
# }

index_still_exists <- function(new, old) {
  get_index_char(old) %in% colnames(new)
}
