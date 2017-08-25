reclass <- function(x, original, ...) {
  UseMethod("reclass")
}

reclass.default <- function(x, original, ...) {

  # Only reclass if index still exists
  if(!check_for_index(x, original)) {
    message("Note: `index` has been removed. Removing `tbl_time` class.")
    return(x)
  }

  setdiff_x    <- setdiff(class(x),class(original))
  setdiff_orig <- setdiff(class(original),class(x))
  overlap      <- intersect(class(x),class(original))

  class(x) <- c(setdiff_orig, setdiff_x, overlap)
  attr(x, "index") <- attr(original, "index")
  attr(x, "time_zone") <- attr(original, "time_zone")
  x
}


declass <- function(x, class_pattern, ...) {
  UseMethod("declass")
}

declass.default <- function(x, class_pattern, ...) {
  classes <- grep(class_pattern, class(x), value = TRUE)
  class(x) <- base::setdiff(class(x), classes)
  x
}

check_for_index <- function(x, original) {
  rlang::quo_name(attr(original, "index")) %in% colnames(x)
}
