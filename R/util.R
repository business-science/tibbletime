# Reexports --------------------------------------------------------------------

#' @importFrom magrittr %>%
#' @export
#'
magrittr::`%>%`

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

make_time_formula <- function(lhs = NULL, rhs) {
  lhs <- validate_side(lhs)
  rhs <- validate_side(rhs)
  formula(paste0(lhs, "~", rhs))
}
