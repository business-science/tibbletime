# ------------------------------------------------------------------------------
# Lags

lags <- function(x, n = 1, ..., default = NA) {
  UseMethod("lags")
}

lags.default <- function(x, n = 1, ..., default = NA) {
  classes <- paste(class(x), collapse = ", ")
  glue_stop("No supported method of lags() for class: ", classes)
}

lags.data.frame <- function(x, n = 1, ..., default = NA){
  vars <- extract_vars(x, ...)

  # For each var to lag, create the list of calls
  calls_list <- purrr::map(vars, ~map_quos(.x, n, default, "lag"))

  # The above results in a list of lists. Flatten to get 1 list of calls
  calls <- rlang::flatten(calls_list)

  dplyr::mutate(x, !!! calls)
}

lags.tbl <- lags.data.frame


# ------------------------------------------------------------------------------
# Leads

leads <- function(x, n = 1, ..., default = NA) {
  UseMethod("leads")
}

leads.default <- function(x, n = 1, ..., default = NA) {
  classes <- paste(class(x), collapse = ", ")
  glue_stop("No supported method of leads() for class: ", classes)
}

leads.data.frame <- function(x, n = 1, ..., default = NA){
  vars <- extract_vars(x, ...)

  # For each var to lead, create the list of calls
  calls_list <- purrr::map(vars, ~map_quos(.x, n, default, "lead"))

  # The above results in a list of lists. Flatten to get 1 list of calls
  calls <- rlang::flatten(calls_list)

  dplyr::mutate(x, !!! calls)
}

leads.tbl <- leads.data.frame


# ------------------------------------------------------------------------------
# Utils

# For each n, create a named list element holding the call
map_quos <- function(var, indices, default, lag_lead) {

  # For each index to lag/lead, create a quo call
  if(lag_lead == "lag") {
    lag <- dplyr::lag
    quo_list <- purrr::map(indices, ~rlang::quo(lag(!!var, !!.x, !!default)))
  } else if(lag_lead == "lead") {
    lead <- dplyr::lead
    quo_list <- purrr::map(indices, ~rlang::quo(lead(!!var, !!.x, !!default)))
  }

  # Create and set names
  fmt <- paste0("%s_", lag_lead, "_%02d")
  nms <- sprintf(fmt, as.character(var), indices)

  rlang::set_names(quo_list, nms)
}

extract_vars <- function(x, ...) {
  vars_char <- tidyselect::vars_select(colnames(x), !!! rlang::enquos(...))
  vars_syms <- rlang::syms(unname(vars_char))
  vars_syms
}

# A glue version of stop()
glue_stop <- function(..., .sep = "") {
  stop(glue::glue(..., .sep, .envir = parent.frame()), call. = FALSE)
}
