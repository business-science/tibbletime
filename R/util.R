# Reexports --------------------------------------------------------------------

#' @importFrom dplyr %>%
#' @export
#'
dplyr::`%>%`

#' @importFrom rlang :=
#'
NULL

#' @importFrom rlang .data
#'
NULL

#' @importFrom rlang %||%
#'
NULL

#' @useDynLib tibbletime, .registration = TRUE
#' @importFrom Rcpp sourceCpp
NULL

# Global util ------------------------------------------------------------------

# A glue version of stop()
glue_stop <- function(..., .sep = "") {
  stop(glue::glue(..., .sep, .envir = parent.frame()), call. = FALSE)
}

# Cheaply get the length of a string
string_length <- function(x) {
  split <- unlist(strsplit(x, ""))
  length(split)
}

index_attributes <- function() {
  c("index_quo", "index_time_zone")
}

make_dummy_dispatch_obj <- function(x) {
  structure(list(), class = x)
}

remove_time_group <- function(x) {
  if(".time_group" %in% colnames(x)) {
    x[[".time_group"]] <- NULL
  }
  x
}
