#' Nest a `tbl_time` to a specified time period
#'
#' @export
#'
time_nest <- function(data, ..., period = "yearly", .key = data) {
  UseMethod("time_nest")
}

#' @export
#'
time_nest.tbl_time <- function(data, ..., period = "yearly", .key = data) {

  # Setup
  cols_not_nested <- retrieve_index(data, as_name = TRUE)
  key_col         <- rlang::enquo(.key)

  # Collapse. Keeping sep column for the old dates
  data_coll <- time_collapse(data, period, as_sep_col = TRUE)

  data_coll %>%

    # Nest all non groups and non index cols
    tidyr::nest_(key_col   = rlang::quo_name(key_col),
                 nest_cols = setdiff(colnames(data_coll), cols_not_nested)) %>%

    # Each element in the nest should be a tbl_time
    dplyr::mutate(!! rlang::quo_name(key_col) := purrr::map(!! key_col, ~as_tbl_time(.x, .date)))
}

#' @export
#'
time_nest.grouped_tbl_time <- function(data, ..., period = "yearly", .key = data) {

  # Setup
  cols_not_nested <- c(dplyr::group_vars(data), retrieve_index(data, as_name = TRUE))
  key_col         <- rlang::enquo(.key)

  # Collapse. Keeping sep column for the old dates
  data_coll <- time_collapse(data, period, as_sep_col = TRUE)

  data_coll %>%

    # Ungroup groups. nest_cols argument takes care of them
    ungroup() %>%

    # Nest all non groups and non index cols
    tidyr::nest_(key_col   = rlang::quo_name(key_col),
                 nest_cols = setdiff(colnames(data_coll), cols_not_nested)) %>%

    # Each element in the nest should be a tbl_time
    dplyr::mutate(!! rlang::quo_name(key_col) := purrr::map(!! key_col, ~as_tbl_time(.x, .date)))
}
