parse_time_formula <- function(index, time_formula) {

  # lhs/rhs list
  tf <- list(
    lhs = rlang::f_lhs(time_formula),
    rhs = rlang::f_rhs(time_formula)
  )

  # Environment to evaluate the sides in
  tf_env <- rlang::f_env(time_formula)

  # Tidy evaluation
  tf <- lapply(tf, function(x) {
    rlang::eval_tidy(x, env = tf_env)
    }
  )

  # Double up if 1 sided
  # length = 2 means that it has ~ and 1 side
  if(length(time_formula) == 2) {
    tf$lhs <- tf$rhs
  }

  tf <- lapply(tf, FUN = function(x) keyword_parse(index, x))

  # Split the input
  tf <- lapply(tf, split_to_list)

  # Add default times
  # map2 is a bit slow here
  tf_final      <- list(NA, NA)
  tf_final[[1]] <- add_time_defaults(index, tf[[1]], "lhs")
  tf_final[[2]] <- add_time_defaults(index, tf[[2]], "rhs")

  tf_final
}

### Utils ----


## Functions for adding defaults ------------------------------------------

# Adds default times to fill out the sides of the time formula
add_time_defaults <- function(index, tf_side, side = "lhs") {

  # Lookup specific index class defaults
  defaults <- lookup_defaults(index, side)

  # Check length
  if(length(tf_side) > length(defaults)) {
    index_class <- class(index)[[1]]
    default_names <- paste(names(defaults), collapse = ", ")
    stop(paste0("For a ", index_class, " index, time_formula can only include ",
                default_names, " specifications."), call. = FALSE)
  }

  # Overwrite defaults where necessary
  for(i in seq_along(tf_side)) {
    defaults[[i]] <- tf_side[[i]]
  }

  # Handle end of month
  if(!is.null(defaults$d)) { # If this passes it was Date/POSIX
    if(defaults$d == 0) {
      # Fake a date to find the number of days in that month
      fake_date <- lubridate::make_date(defaults$y, defaults$m, 1)
      defaults$d <- lubridate::days_in_month(fake_date)
    }
  }

  defaults
}

## Functions for keyword parsing ------------------------------------------

keyword_parse <- function(index, side) {

  # Dummy index
  if(length(index) == 0) {
    return(side)
  }

  if(as.character(side) == "start") {
    dplyr::first(index)
  } else if (as.character(side) == "end") {
    dplyr::last(index)
  } else {
    side
  }

}
