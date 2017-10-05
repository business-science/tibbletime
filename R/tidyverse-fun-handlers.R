# tidyverse_execute ------------------------------------------------------------

# tidyverse_execute() executes any dplyr/tidyr/purrr function
# on a tbl_time object, in the process retaining
# time based classes and attributes that would normally be stripped
tidyverse_execute <- function(.x, fun, ..., silent_retime = FALSE) {
  UseMethod("tidyverse_execute")
}

tidyverse_execute.default <- function(.x, fun, ..., silent_retime = FALSE) {
  stop("Object is not of class `tbl_time`.")
}

tidyverse_execute.tbl_time <- function(.x, fun, ..., silent_retime = FALSE) {

  # Assert that attributes still exist
  assertthat::assert_that(!is.null(attr(.x, "index")),
    msg = "`index` attribute is `NULL`. Was it removed by a function call?")
  assertthat::assert_that(!is.null(attr(.x, "time_zone")),
    msg = "`time_zone` attribute is `NULL`. Was it removed by a function call?")

  # Classes and attributes to keep
  time_classes <- extract_time_classes(.x)
  time_attrs   <- extract_time_attrs(.x)

  # Remove, execute dplyr fun, add back
  .x %>%
    detime(time_classes, time_attrs) %>%
    fun(...) %>%
    retime(time_classes, time_attrs, silent_retime)
}

# retime -----------------------------------------------------------------------

# retime() adds time based classes and attributes after a tidyverse manipulation
retime <- function(x, time_classes, time_attrs,
                   silent_retime = FALSE, ...) {
  UseMethod("retime")
}

retime.default <- function(x, time_classes, time_attrs,
                           silent_retime = FALSE, ...) {

  # Only retime if index still exists
  if(!check_for_index(x, time_attrs)) {
    if(!silent_retime) {
      message("Note: `index` has been removed. Removing `tbl_time` class.")
    }
    return(x)
  }

  class(x) <- unique(c(time_classes, class(x)))
  attributes(x) <- c(attributes(x), time_attrs)

  x
}

# detime -----------------------------------------------------------------------

# detime() strips time based classes and attributes
# before a tidyverse manipulation
detime <- function(x, time_classes, time_attrs, ...) {
  UseMethod("detime")
}

detime.default <- function(x, time_classes, time_attrs, ...) {

  # Get names of the attributes
  time_attrs_names <- rlang::names2(time_attrs)

  # Strip time classes
  class(x) <- base::setdiff(class(x), time_classes)

  # Strip time attributes
  for(i in time_attrs_names) {
    attr(x, i) <- NULL
  }

  x
}

# Utils ------------------------------------------------------------------------

check_for_index <- function(x, time_attrs) {
  rlang::quo_name(time_attrs[["index"]]) %in% colnames(x)
}

extract_time_classes <- function(x) {
  stringr::str_subset(class(x), "tbl_time")
}

extract_time_attrs <- function(x) {
  list(
    index     = attr(x, "index"),
    time_zone = attr(x, "time_zone")
  )
}
