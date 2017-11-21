parse_time_formula <- function(index, time_formula) {

  # Double up if 1 sided
  # length = 2 means that it has ~ and 1 side
  if(length(time_formula) == 2) {
    .rhs <- rlang::f_rhs(time_formula)
    .lhs <- .rhs
    .env <- rlang::f_env(time_formula)
    time_formula <- rlang::new_formula(.lhs, .rhs, .env)
  }

  # Split time formula into lhs and rhs and list() each
  tf_list <- rlang::lang_args(time_formula)
  tf_list <- lapply(tf_list, list)

  # Split each formula side into it's time pieces
  tf_fully_split <- lapply(tf_list, lang_args_recursive)

  # Add default times
  # map2 is a bit slow here
  tf_final    <- list(NA, NA)
  tf_final[[1]] <- add_time_defaults(index, tf_fully_split[[1]], "lhs")
  tf_final[[2]] <- add_time_defaults(index, tf_fully_split[[2]], "rhs")

  tf_final
}

#### Utils ---------------------------------------------------------------------

## Functions for recursively splitting the formula sides --------------------

# Used to recursively apply lang_args to 1 side of the time formula
# End up with a flat list of split time pieces
lang_args_recursive <- function(lang) {
  while(needs_recursion(lang)) {
    lang <- map_lang_args(lang)
  }
  lang
}

# If any of the lang sides are longer than 1, it needs splitting
needs_recursion <- function(lang) {
  sum(
    vapply(
      lang, 
      function(x) length(x) > 1, 
      FUN.VALUE = logical(1)
    )
  )
}

# For each tf side, apply a lang_args call
map_lang_args <- function(lang) {
  unlist(lapply(lang, safe_lang_args), recursive = FALSE)
}

# A safer version of lang_args for this case
# Safer because if it is not a lang, it returns a list of itself which
# is what is needed for this to flatten correctly
safe_lang_args <- function(lang) {
  if(rlang::is_lang(lang)) {
    rlang::lang_args(lang)
  } else {
    list(lang)
  }
}

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

