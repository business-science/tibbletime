#' Apply a function to each element of a `tbl_time` object.
#'
#' The tmap functions transform a `tbl_time` input by applying a function to
#' each column at a specified time interval.
#'
#' @details
#'
#' These functions are similar to [purrr::map()],
#' but allow the user to also perform grouped mapping over
#' intervals such as `"yearly"`, `"monthly"`, `"hourly"`, etc. For example,
#' if `"monthly"` is chosen, one could picture the `tbl_time` object being
#' split into smaller `tbl_time`s, one for each month, and having the function
#' mapped over all of the columns in each of those smaller tibbles. The results
#' are then recombined into one tibble, with a list-column holding the results
#' of the mapping over each time interval.
#'
#' Groupings applied using [dplyr::group_by()] are respected.
#'
#' @return
#'
#' A `tbl_time` object grouped by the time interval specified. The last
#' available date in that interval is returned as the new date.
#'
#'
#' @inheritParams purrr::map
#' @param .x A `tbl_time` object
#' @param period A period to group the mapping by
#' @param name The name of the list-column generated
#'
#' @rdname tmap
#'

##### tmap ---------------------------------------------------------------------

#' @export
#' @rdname tmap
#'
tmap <- function(.x, .f, period = "yearly", name = NULL, ...) {
  UseMethod("tmap")
}

#' @export
tmap.tbl_time <- function(.x, .f, period = "yearly", name = NULL, ...) {
  join_cols <- retrieve_index(.x, as_name = TRUE)
  name <- rlang::enquo(name)
  tmap_variant(.x, .f, period = period, name = !! name,
               map_type = purrr::map, join_cols = join_cols, ...)
}

#' @export
tmap.grouped_tbl_time <- function(.x, .f, period = "yearly", name = NULL, ...) {
  join_cols <- c(dplyr::group_vars(.x), retrieve_index(.x, as_name = TRUE))
  name <- rlang::enquo(name)
  x <- tmap_variant(.x, .f, period = period, name = !! name,
                    map_type = purrr::map, join_cols = join_cols, ...)
  group_by(x, !!! dplyr::groups(.x))
}

##### tmap_dbl -----------------------------------------------------------------

#' @export
#' @rdname tmap
#'
tmap_chr <- function(.x, .f, period = "yearly", name = NULL, ...) {
  UseMethod("tmap_chr")
}

#' @export
tmap_chr.tbl_time <- function(.x, .f, period = "yearly", name = NULL, ...) {
  join_cols <- retrieve_index(.x, as_name = TRUE)
  name <- rlang::enquo(name)
  tmap_variant(.x, .f, period = period, name = !! name,
               map_type = purrr::map_chr, join_cols = join_cols, ...)
}

#' @export
tmap_chr.grouped_tbl_time <- function(.x, .f, period = "yearly", name = NULL, ...) {
  join_cols <- c(dplyr::group_vars(.x), retrieve_index(.x, as_name = TRUE))
  name <- rlang::enquo(name)
  x <- tmap_variant(.x, .f, period = period, name = !! name,
                    map_type = purrr::map_chr, join_cols = join_cols, ...)
  group_by(x, !!! dplyr::groups(.x))
}

##### tmap_int -----------------------------------------------------------------

#' @export
#' @rdname tmap
#'
tmap_int <- function(.x, .f, period = "yearly", name = NULL, ...) {
  UseMethod("tmap_int")
}

#' @export
tmap_int.tbl_time <- function(.x, .f, period = "yearly", name = NULL, ...) {
  join_cols <- retrieve_index(.x, as_name = TRUE)
  name <- rlang::enquo(name)
  tmap_variant(.x, .f, period = period, name = !! name,
               map_type = purrr::map_int, join_cols = join_cols, ...)
}

#' @export
tmap_int.grouped_tbl_time <- function(.x, .f, period = "yearly", name = NULL, ...) {
  join_cols <- c(dplyr::group_vars(.x), retrieve_index(.x, as_name = TRUE))
  name <- rlang::enquo(name)
  x <- tmap_variant(.x, .f, period = period, name = !! name,
                    map_type = purrr::map_int, join_cols = join_cols, ...)
  group_by(x, !!! dplyr::groups(.x))
}

##### tmap_lgl -----------------------------------------------------------------

#' @export
#' @rdname tmap
#'
tmap_lgl <- function(.x, .f, period = "yearly", name = NULL, ...) {
  UseMethod("tmap_lgl")
}

#' @export
tmap_lgl.tbl_time <- function(.x, .f, period = "yearly", name = NULL, ...) {
  join_cols <- retrieve_index(.x, as_name = TRUE)
  name <- rlang::enquo(name)
  tmap_variant(.x, .f, period = period, name = !! name,
               map_type = purrr::map_lgl, join_cols = join_cols, ...)
}

#' @export
tmap_lgl.grouped_tbl_time <- function(.x, .f, period = "yearly", name = NULL, ...) {
  join_cols <- c(dplyr::group_vars(.x), retrieve_index(.x, as_name = TRUE))
  name <- rlang::enquo(name)
  x <- tmap_variant(.x, .f, period = period, name = !! name,
                    map_type = purrr::map_lgl, join_cols = join_cols, ...)
  group_by(x, !!! dplyr::groups(.x))
}

##### tmap_dbl -----------------------------------------------------------------

#' @export
#' @rdname tmap
#'
tmap_dbl <- function(.x, .f, period = "yearly", name = NULL, ...) {
  UseMethod("tmap_dbl")
}

#' @export
tmap_dbl.tbl_time <- function(.x, .f, period = "yearly", name = NULL, ...) {
  join_cols <- retrieve_index(.x, as_name = TRUE)
  name <- rlang::enquo(name)
  tmap_variant(.x, .f, period = period, name = !! name,
               map_type = purrr::map_dbl, join_cols = join_cols, ...)
}

#' @export
tmap_dbl.grouped_tbl_time <- function(.x, .f, period = "yearly", name = NULL, ...) {
  join_cols <- c(dplyr::group_vars(.x), retrieve_index(.x, as_name = TRUE))
  name <- rlang::enquo(name)
  x <- tmap_variant(.x, .f, period = period, name = !! name,
                    map_type = purrr::map_dbl, join_cols = join_cols, ...)
  group_by(x, !!! dplyr::groups(.x))
}

##### tmap_dfc -----------------------------------------------------------------

#' @export
#' @rdname tmap
#'
tmap_dfc <- function(.x, .f, period = "yearly", name = NULL, ...) {
  UseMethod("tmap_dfc")
}

#' @export
tmap_dfc.tbl_time <- function(.x, .f, period = "yearly", name = NULL, ...) {
  join_cols <- retrieve_index(.x, as_name = TRUE)
  name <- rlang::enquo(name)
  tmap_variant(.x, .f, period = period, name = !! name,
               map_type = purrr::map_dfc, join_cols = join_cols, ...)
}

#' @export
tmap_dfc.grouped_tbl_time <- function(.x, .f, period = "yearly", name = NULL, ...) {
  join_cols <- c(dplyr::group_vars(.x), retrieve_index(.x, as_name = TRUE))
  name <- rlang::enquo(name)
  x <- tmap_variant(.x, .f, period = period, name = !! name,
                    map_type = purrr::map_dfc, join_cols = join_cols, ...)
  group_by(x, !!! dplyr::groups(.x))
}

##### tmap_dfr -----------------------------------------------------------------

#' @export
#' @rdname tmap
#'
tmap_dfr <- function(.x, .f, period = "yearly", name = NULL, ...) {
  UseMethod("tmap_dfr")
}

#' @export
tmap_dfr.tbl_time <- function(.x, .f, period = "yearly", name = NULL, ...) {
  join_cols <- retrieve_index(.x, as_name = TRUE)
  name <- rlang::enquo(name)
  tmap_variant(.x, .f, period = period, name = !! name,
               map_type = purrr::map_dfr, join_cols = join_cols, ...)
}

#' @export
tmap_dfr.grouped_tbl_time <- function(.x, .f, period = "yearly", name = NULL, ...) {
  join_cols <- c(dplyr::group_vars(.x), retrieve_index(.x, as_name = TRUE))
  name <- rlang::enquo(name)
  x <- tmap_variant(.x, .f, period = period, name = !! name,
                    map_type = purrr::map_dfr, join_cols = join_cols, ...)
  group_by(x, !!! dplyr::groups(.x))
}

##### tmap_df -----------------------------------------------------------------

#' @export
#' @rdname tmap
#'
tmap_df <- function(.x, .f, period = "yearly", name = NULL, ...) {
  UseMethod("tmap_df")
}

#' @export
tmap_df.tbl_time <- function(.x, .f, period = "yearly", name = NULL, ...) {
  join_cols <- retrieve_index(.x, as_name = TRUE)
  name <- rlang::enquo(name)
  tmap_variant(.x, .f, period = period, name = !! name,
               map_type = purrr::map_df, join_cols = join_cols, ...)
}

#' @export
tmap_df.grouped_tbl_time <- function(.x, .f, period = "yearly", name = NULL, ...) {
  join_cols <- c(dplyr::group_vars(.x), retrieve_index(.x, as_name = TRUE))
  name <- rlang::enquo(name)
  x <- tmap_variant(.x, .f, period = period, name = !! name,
                    map_type = purrr::map_df, join_cols = join_cols, ...)
  group_by(x, !!! dplyr::groups(.x))
}

##### Util ---------------------------------------------------------------------

tmap_variant <- function(.x, .f,
                         period = "yearly",
                         map_type = NULL,
                         join_cols = NULL,
                         name = NULL, ...) {

  # Get the key column name
  key_col <- rlang::enquo(name)
  if(rlang::quo_is_null(key_col)) {
    key_col <- rlang::sym("data")
  }

  # Collapse to the correct period
  time_collapse(.x, period) %>%

    # Ungroup. nest_cols argument handles them
    ungroup() %>%

    # Nest
    tidyr::nest_(key_col   = rlang::quo_name(key_col),
                 nest_cols = setdiff(colnames(.x), join_cols)) %>%

    # Map function to each group's data frame
    dplyr::mutate(!! rlang::quo_name(key_col) := purrr::map(.x = !! key_col,
                                                            .f = ~map_type(.x, .f)))

}
