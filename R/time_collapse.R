#' Collapse a `tbl_time` object so that all observations in a period share the
#' same date
#'
#' @export
time_collapse <- function(.data, period = "yearly", as_sep_col = FALSE, ...) {
  UseMethod("time_collapse")
}

#' @export
time_collapse.tbl_time <- function(.data, period = "yearly", as_sep_col = FALSE, ...) {
  join_cols <- retrieve_index(.data, as_name = TRUE)
  collapse_it(.data, period = period, join_cols = join_cols, as_sep_col = as_sep_col, ...)
}

#' @export
time_collapse.grouped_tbl_time <- function(.data, period = "yearly", as_sep_col = FALSE, ...) {
  join_cols <- c(group_vars(.data), retrieve_index(.data, as_name = TRUE))
  collapse_it(.data, period = period, join_cols = join_cols, as_sep_col = as_sep_col, ...)
}

# Utils -----

collapse_it <- function(.data, period = "yearly", join_cols, as_sep_col = FALSE, ...) {

  index          <- retrieve_index(.data)
  index_char     <- retrieve_index(.data, as_name = TRUE)
  index_sym      <- rlang::sym(index_char)
  exp_index      <- expand_index(.data, period)

  # Need join_cols = combo of groups and index
  period_cols    <- setdiff(x = colnames(exp_index), y = join_cols)

  # Keep the original dates as .date if requested
  if(as_sep_col) {
    .data <- dplyr::mutate(.data, !! rlang::sym(".date") := !! index_sym) %>%
      select(join_cols, .date, everything())
  }

  # Join .data with expanded index
  dplyr::left_join(.data, exp_index, by = join_cols) %>%

    # Additionally group by expanded index periods
    dplyr::group_by(!!! rlang::syms(period_cols), add = TRUE) %>%

    # Index column becomes the max of that group
    # Keep the original dates as .date if requested
    dplyr::mutate(!! index_sym := max(!! index_sym)) %>%

    # Regroup by original groups (removes period grouping)
    dplyr::group_by(!!! dplyr::groups(.data)) %>%

    # Remove period cols
    dplyr::select(- one_of(period_cols))
}
