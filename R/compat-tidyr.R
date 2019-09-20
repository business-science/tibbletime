nest.tbl_time <- function(.data, ..., .key = "DEPRECATED") {
  check_tidyr_version()

  .key       <- rlang::enexpr(.key)
  .key_sym   <- rlang::sym(.key)
  .key_char  <- rlang::expr_name(.key)
  index_quo  <- get_index_quo(.data)
  index_char <- get_index_char(.data)

  # Need this to avoid data VS .key = "data" collision in the mutate/map
  ..original_data <- .data

  # Perform the nest on a tibble
  .data_nested <- tidyr::nest(as_tibble(.data), ..., .key = .key_char)

  # Figure out the names of the new nested columns
  if (.key == "DEPRECATED") {
    nested_columns <- names(rlang::enquos(...))

    if (rlang::is_empty(nested_columns)) {
      nested_columns <- "data"
    }
  } else {
    nested_columns <- .key_char
  }

  contains_index <- function(col) {
    index_char %in% colnames(.data_nested[[col]][[1]])
  }

  index_is_nested <- vapply(nested_columns, contains_index, logical(1))

  for (i in seq_along(nested_columns)) {
    # Each nested element should be a list_of<tbl_time> with attributes
    if (index_is_nested[i]) {
      nested_column_sym <- rlang::sym(nested_columns[i])

      .data_nested <- dplyr::mutate(
        .data_nested,
        !!nested_column_sym := purrr::map(!!nested_column_sym, ~reconstruct(.x, ..original_data)),
        !!nested_column_sym := vctrs::as_list_of(!!nested_column_sym, .ptype = (!!nested_column_sym)[[1]])
      )
    } else {
      # The index is in the outer df
      .data_nested <- reconstruct(.data_nested, ..original_data)
    }
  }

  .data_nested
}

unnest.tbl_time <- function(data,
                            cols,
                            ...,
                            keep_empty = FALSE,
                            ptype = NULL,
                            names_sep = NULL,
                            names_repair = "check_unique",
                            .drop = "DEPRECATED",
                            .id = "DEPRECATED",
                            .sep = "DEPRECATED",
                            .preserve = "DEPRECATED") {
  check_tidyr_version()

  # This is called after nesting but excluding the index in the nest
  #reconstruct(NextMethod(), data)

  # Pre-evaluate `cols`, as NextMethod() will evaluate it before tidyr can enquo() it
  cols <- tidyselect::vars_select(names(data), !!rlang::enquo(cols))

  copy_.data <- new_tbl_time(data, get_index_quo(data), get_index_time_zone(data))
  reconstruct(NextMethod(), copy_.data)
}

unnest.tbl_df <- function(data,
                          cols,
                          ...,
                          keep_empty = FALSE,
                          ptype = NULL,
                          names_sep = NULL,
                          names_repair = "check_unique",
                          .drop = "DEPRECATED",
                          .id = "DEPRECATED",
                          .sep = "DEPRECATED",
                          .preserve = "DEPRECATED") {
  check_tidyr_version()
  # Called after nesting a tbl_time, index is in the nest, then unnesting

  # Pre-evaluate `cols`, as NextMethod() will evaluate it before tidyr can enquo() it
  cols <- tidyselect::vars_select(names(data), !!rlang::enquo(cols))

  list_cols <- names(data)[purrr::map_lgl(data, rlang::is_list)]

  # If any contain a tbl_time, special handling
  list_col_is_tbl_time <- purrr::map_lgl(
    .x = list_cols,
    .f = ~inherits(data[[.x]][[1]], "tbl_time")
  )

  contains_inner_tbl_time <- any(list_col_is_tbl_time)
  contains_outer_tbl_time <- inherits(data, "tbl_time")

  # Inner is tbl_time, but the outer tbl is not one. Want to maintain
  # tbl_time class
  if(contains_inner_tbl_time & !contains_outer_tbl_time) {

    # Grab nested columns
    nested <- dplyr::transmute(dplyr::ungroup(data), !!! rlang::syms(list_cols))

    # Which list columns contain tbl_time objects? Extract the first one
    # to reconstruct with
    which_tbl_time <- which(list_col_is_tbl_time)

    which_tbl_time <- which_tbl_time[1]
    nested_time <- nested[[which_tbl_time]][[1]]

    unnested_data <- NextMethod()

    reconstruct(unnested_data, nested_time)

  } else (
    # No special handling, pass on to unnest()
    NextMethod()
  )
}


# ------------------------------------------------------------------------------
# gather() and spread() seem to be needed as well

gather.tbl_time <- function(data, key = "key", value = "value", ..., na.rm = FALSE,
                            convert = FALSE, factor_key = FALSE)  {
  key   <- rlang::enquo(key)
  value <- rlang::enquo(value)
  quos  <- rlang::quos(...)

  gathered_data <- tidyr::gather(as_tibble(data), key = !! key, value = !! value, !!! quos,
                          na.rm = na.rm, convert = convert, factor_key = factor_key)

  reconstruct(gathered_data, data)
}

spread.tbl_time <- function(data, key, value, fill = NA, convert = FALSE, drop = TRUE,
                            sep = NULL)  {
  key   <- rlang::enquo(key)
  value <- rlang::enquo(value)

  spread_data <- tidyr::spread(as_tibble(data), key = !! key, value = !! value,
                        fill = fill, convert = convert, drop = drop,
                        sep = sep)

  reconstruct(spread_data, data)
}

# ------------------------------------------------------------------------------

check_tidyr_version <- function() {
  if (tidyr_at_least_1.0.0) {
    return()
  }

  rlang::abort("`tidyr` must be at least version '1.0.0' to use this feature.")
}


