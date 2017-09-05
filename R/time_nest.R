time_nest <- function(data, ..., period = "yearly", .key = data) {

  index          <- retrieve_index(data)
  index_char     <- retrieve_index(data, as_name = TRUE)
  index_sym      <- rlang::sym(index_char)
  exp_index      <- expand_index(data, period)
  key_col        <- enquo(.key)

  if(rlang::quo_is_null(key_col)) {
    key_col <- rlang::sym("data")
  }

  # Need join_cols = combo of groups and index
  period_cols    <- setdiff(x = colnames(exp_index), y = join_cols)

  # Join data with expanded index
  dplyr::left_join(data, exp_index, by = join_cols) %>%

    # Additionally group by expanded index periods
    dplyr::group_by(!!! rlang::syms(period_cols), add = TRUE) %>%

    # Index column becomes the max of that group
    dplyr::mutate(!! index_sym := max(!! index_sym)) %>%

    # Regroup by original join_cols (removes period grouping)
    dplyr::group_by(!!! rlang::syms(join_cols)) %>%

    # Remove period cols
    dplyr::select(- one_of(period_cols)) %>%

    # Nest
    tidyr::nest_(key_col   = rlang::quo_name(key_col),
                 nest_cols = setdiff(colnames(data), join_cols))

}
