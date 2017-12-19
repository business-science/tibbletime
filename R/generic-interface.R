# Generic functions that define the differences between date/time classes

# Checks if the input is a supported date/time class
inherits_allowed_datetime <- function(x) {
  inherits(x, c("Date", "POSIXct", "POSIXt", "yearmon", "yearqtr", "hms"))
}

#### General -------------------------------------------------------------------


#### create_series -------------------------------------------------------------

# For sequence creation in create_series()
lookup_seq_fun <- function(x) {
  UseMethod("lookup_seq_fun")
}

# Really only necessary because c(hms, hms) loses the hms class
push_datetime <- function(x, push) {
  UseMethod("push_datetime")
}

push_datetime.default <- function(x, push) {
  unique(c(x, push))
}


#### parse_period --------------------------------------------------------------

# Check that the supplied period formula is allowed for that class
assert_period_matches_index_class <- function(x, period) {
  UseMethod("assert_period_matches_index_class")
}

assert_period_matches_index_class.default <- function(x, period) {
  glue_stop("Class '{class(x)}' is not a known index class.")
}


#### parse_time_formula --------------------------------------------------------

# Find the default time_formula list values. These get overwritten
# with the user supplied values
lookup_defaults <- function(index, side = "lhs") {
  UseMethod("lookup_defaults")
}

# Collapse the list of period values into a real datetime class
list_to_datetime <- function(index, tf_side, ...) {
  UseMethod("list_to_datetime")
}

#### time_collapse -------------------------------------------------------------

# This goes with dispatch_max_collapse
# The idea is to collapse a date column to its maximum date per group
# Special handling for yearmon
max_collapse <- function(x) {
  dummy_obj <- get_index_dispatcher(x)
  dispatch_max_collapse(dummy_obj, x)
}

# This dispatches the collapse based on the index class
dispatch_max_collapse <- function(dummy, x) {
  UseMethod("dispatch_max_collapse")
}

dispatch_max_collapse.default <- function(dummy, x) {
  index_char      <- get_index_char(x)
  index_sym       <- rlang::sym(index_char)

  dplyr::mutate(x, !! index_sym := max(!! index_sym))
}

#### time_group ----------------------------------------------------------------

# Coerce a character start_date to a real datetime
coerce_start_date <- function(x, start_date) {
  UseMethod("coerce_start_date")
}
