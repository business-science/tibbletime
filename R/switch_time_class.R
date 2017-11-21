switch_time_class <- function(x, class = "POSIXct") {

  is_allowed_datetime(x)

  if(class(x)[[1]] == class) {
    return(x)
  }

  .index <- to_posixct_numeric(x)

  new_index <- posixct_numeric_to_datetime(
    .index,
    class = class,
    tz = get_index_col_time_zone(x)
  )

  new_index
}
