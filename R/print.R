#' @export
#' @importFrom pillar tbl_sum
tbl_sum.tbl_time <- function(x) {
  out <- c(
    "A time tibble" = pillar::dim_desc(x),
    "Index" = get_index_char(x)
  )

  if (dplyr::is_grouped_df(x)) {
    out <- c(out, "Groups" = group_sum(x))
  }

  out
}

# `dplyr:::group_sum()`
group_sum <- function(x) {
  grps <- dplyr::n_groups(x)

  vars <- dplyr::group_vars(x)
  vars <- paste0(vars, collapse = ", ")

  paste0(vars, " [", big_mark(grps), "]")
}

# `dplyr:::big_mark()`
big_mark <- function(x, ...) {
  mark <- if (identical(getOption("OutDec"), ",")) {
    "."
  } else {
    ","
  }
  formatC(x, big.mark = mark, ...)
}
