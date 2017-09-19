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
#' of the mapping over each time period.
#'
#' Groupings applied using [dplyr::group_by()] are respected.
#'
#' @note
#'
#' The following periods are available:
#' * `"yearly"`
#' * `"quarterly"`
#' * `"monthly"`
#' * `"weekly"`
#' * `"daily"`
#' * `"hour"`
#' * `"minute"`
#' * `"second"`
#'
#' @return
#'
#' A `tbl_time` object grouped by the time interval specified. The last
#' available date in that interval is returned as the new date.
#'
#' @inheritParams purrr::map
#' @param .x A `tbl_time` object.
#' @param period A period to group the mapping by.
#' @param name The character name of the list-column generated.
#'
#' @examples
#'
#' # First example -------------------------------------------------------------
#'
#' data(FB)
#' FB <- as_tbl_time(FB, date)
#' # No need for the symbol column here
#' FB <- dplyr::select(FB, -symbol)
#'
#' # Get the yearly average of every column in FB
#' mapped_mean <- FB %>%
#'   tmap(.f = ~mean(.x), period = "yearly")
#'
#' # It is returned as a list-column because the time period adds
#' # an extra dimension to the mapping
#' mapped_mean
#'
#' # Access individual elements. Here, the 2013 results
#' mapped_mean$data[[1]]
#'
#' # More useful example -------------------------------------------------------
#'
#' data(FB)
#' FB <- as_tbl_time(FB, date)
#' # No need for the symbol column here
#' FB <- dplyr::select(FB, -symbol)
#'
#' # An easier approach might be to use `tmap_dfc` to coerce each list-column
#' # entry to a tibble, then unnest the result
#' # Here we calculate the monthly average for each column
#' FB %>%
#'   tmap_dfc(~mean(.x), period = "monthly") %>%
#'   tidyr::unnest()
#'
#' # Functions with multiple return values -------------------------------------
#'
#' data(FB)
#' FB <- as_tbl_time(FB, date)
#' # No need for the symbol column here
#' FB <- dplyr::select(FB, -symbol)
#'
#' # Functions that return more than 1 number per map are possible, but more
#' # difficult to work with.
#' # Mapping the quantile function to each column of FB at yearly time increments
#' mapped_quantile <- FB %>%
#'   tmap(~quantile(.x), "yearly")
#'
#' mapped_quantile$data[[1]]
#'
#' # It is possible to get a cleaner result, but currently not intuitive
#' library(tibble)
#' FB %>%
#'   tidyr::gather(key = col_type, value = value, -date) %>%
#'   dplyr::group_by(col_type) %>%
#'   tmap_dfr(~quantile(.x) %>% as.list %>% as.tibble, period = "yearly") %>%
#'   tidyr::unnest()
#'
#' @rdname tmap
#'

##### tmap ---------------------------------------------------------------------

#' @export
#' @rdname tmap
#'
tmap <- function(.x, .f, period = "yearly", name = "data", ...) {
  UseMethod("tmap")
}

#' @export
tmap.default <- function(.x, .f, period = "yearly", name = "data", ...) {
  stop("Object is not of class `tbl_time`.", call. = FALSE)
}

#' @export
tmap.tbl_time <- function(.x, .f, period = "yearly", name = "data", ...) {
  join_cols <- retrieve_index(.x, as_name = TRUE)
  tmap_variant(.x, .f, period = period, name = name,
               map_type = purrr::map, join_cols = join_cols, ...)
}

#' @export
tmap.grouped_tbl_time <- function(.x, .f, period = "yearly", name = "data", ...) {
  join_cols <- c(dplyr::group_vars(.x), retrieve_index(.x, as_name = TRUE))
  x <- tmap_variant(.x, .f, period = period, name = name,
                    map_type = purrr::map, join_cols = join_cols, ...)
  group_by(x, !!! dplyr::groups(.x))
}

##### tmap_dbl -----------------------------------------------------------------

#' @export
#' @rdname tmap
#'
tmap_chr <- function(.x, .f, period = "yearly", name = "data", ...) {
  UseMethod("tmap_chr")
}

#' @export
tmap_chr.default <- function(.x, .f, period = "yearly", name = "data", ...) {
  stop("Object is not of class `tbl_time`.", call. = FALSE)
}

#' @export
tmap_chr.tbl_time <- function(.x, .f, period = "yearly", name = "data", ...) {
  join_cols <- retrieve_index(.x, as_name = TRUE)
  tmap_variant(.x, .f, period = period, name = name,
               map_type = purrr::map_chr, join_cols = join_cols, ...)
}

#' @export
tmap_chr.grouped_tbl_time <- function(.x, .f, period = "yearly", name = "data", ...) {
  join_cols <- c(dplyr::group_vars(.x), retrieve_index(.x, as_name = TRUE))
  x <- tmap_variant(.x, .f, period = period, name = name,
                    map_type = purrr::map_chr, join_cols = join_cols, ...)
  group_by(x, !!! dplyr::groups(.x))
}

##### tmap_int -----------------------------------------------------------------

#' @export
#' @rdname tmap
#'
tmap_int <- function(.x, .f, period = "yearly", name = "data", ...) {
  UseMethod("tmap_int")
}

#' @export
tmap_int.default <- function(.x, .f, period = "yearly", name = "data", ...) {
  stop("Object is not of class `tbl_time`.", call. = FALSE)
}

#' @export
tmap_int.tbl_time <- function(.x, .f, period = "yearly", name = "data", ...) {
  join_cols <- retrieve_index(.x, as_name = TRUE)
  tmap_variant(.x, .f, period = period, name = name,
               map_type = purrr::map_int, join_cols = join_cols, ...)
}

#' @export
tmap_int.grouped_tbl_time <- function(.x, .f, period = "yearly", name = "data", ...) {
  join_cols <- c(dplyr::group_vars(.x), retrieve_index(.x, as_name = TRUE))
  x <- tmap_variant(.x, .f, period = period, name = name,
                    map_type = purrr::map_int, join_cols = join_cols, ...)
  group_by(x, !!! dplyr::groups(.x))
}

##### tmap_lgl -----------------------------------------------------------------

#' @export
#' @rdname tmap
#'
tmap_lgl <- function(.x, .f, period = "yearly", name = "data", ...) {
  UseMethod("tmap_lgl")
}

#' @export
tmap_lgl.default <- function(.x, .f, period = "yearly", name = "data", ...) {
  stop("Object is not of class `tbl_time`.", call. = FALSE)
}

#' @export
tmap_lgl.tbl_time <- function(.x, .f, period = "yearly", name = "data", ...) {
  join_cols <- retrieve_index(.x, as_name = TRUE)
  tmap_variant(.x, .f, period = period, name = name,
               map_type = purrr::map_lgl, join_cols = join_cols, ...)
}

#' @export
tmap_lgl.grouped_tbl_time <- function(.x, .f, period = "yearly", name = "data", ...) {
  join_cols <- c(dplyr::group_vars(.x), retrieve_index(.x, as_name = TRUE))
  x <- tmap_variant(.x, .f, period = period, name = name,
                    map_type = purrr::map_lgl, join_cols = join_cols, ...)
  group_by(x, !!! dplyr::groups(.x))
}

##### tmap_dbl -----------------------------------------------------------------

#' @export
#' @rdname tmap
#'
tmap_dbl <- function(.x, .f, period = "yearly", name = "data", ...) {
  UseMethod("tmap_dbl")
}

#' @export
tmap_dbl.default <- function(.x, .f, period = "yearly", name = "data", ...) {
  stop("Object is not of class `tbl_time`.", call. = FALSE)
}

#' @export
tmap_dbl.tbl_time <- function(.x, .f, period = "yearly", name = "data", ...) {
  join_cols <- retrieve_index(.x, as_name = TRUE)
  tmap_variant(.x, .f, period = period, name = name,
               map_type = purrr::map_dbl, join_cols = join_cols, ...)
}

#' @export
tmap_dbl.grouped_tbl_time <- function(.x, .f, period = "yearly", name = "data", ...) {
  join_cols <- c(dplyr::group_vars(.x), retrieve_index(.x, as_name = TRUE))
  x <- tmap_variant(.x, .f, period = period, name = name,
                    map_type = purrr::map_dbl, join_cols = join_cols, ...)
  group_by(x, !!! dplyr::groups(.x))
}

##### tmap_dfc -----------------------------------------------------------------

#' @export
#' @rdname tmap
#'
tmap_dfc <- function(.x, .f, period = "yearly", name = "data", ...) {
  UseMethod("tmap_dfc")
}

#' @export
tmap_dfc.default <- function(.x, .f, period = "yearly", name = "data", ...) {
  stop("Object is not of class `tbl_time`.", call. = FALSE)
}

#' @export
tmap_dfc.tbl_time <- function(.x, .f, period = "yearly", name = "data", ...) {
  join_cols <- retrieve_index(.x, as_name = TRUE)
  tmap_variant(.x, .f, period = period, name = name,
               map_type = purrr::map_dfc, join_cols = join_cols, ...)
}

#' @export
tmap_dfc.grouped_tbl_time <- function(.x, .f, period = "yearly", name = "data", ...) {
  join_cols <- c(dplyr::group_vars(.x), retrieve_index(.x, as_name = TRUE))
  x <- tmap_variant(.x, .f, period = period, name = name,
                    map_type = purrr::map_dfc, join_cols = join_cols, ...)
  group_by(x, !!! dplyr::groups(.x))
}

##### tmap_dfr -----------------------------------------------------------------

#' @export
#' @rdname tmap
#'
tmap_dfr <- function(.x, .f, period = "yearly", name = "data", ...) {
  UseMethod("tmap_dfr")
}

#' @export
tmap_dfr.default <- function(.x, .f, period = "yearly", name = "data", ...) {
  stop("Object is not of class `tbl_time`.", call. = FALSE)
}

#' @export
tmap_dfr.tbl_time <- function(.x, .f, period = "yearly", name = "data", ...) {
  join_cols <- retrieve_index(.x, as_name = TRUE)
  tmap_variant(.x, .f, period = period, name = name,
               map_type = purrr::map_dfr, join_cols = join_cols, ...)
}

#' @export
tmap_dfr.grouped_tbl_time <- function(.x, .f, period = "yearly", name = "data", ...) {
  join_cols <- c(dplyr::group_vars(.x), retrieve_index(.x, as_name = TRUE))
  x <- tmap_variant(.x, .f, period = period, name = name,
                    map_type = purrr::map_dfr, join_cols = join_cols, ...)
  group_by(x, !!! dplyr::groups(.x))
}

##### tmap_df -----------------------------------------------------------------

#' @export
#' @rdname tmap
#'
tmap_df <- function(.x, .f, period = "yearly", name = "data", ...) {
  UseMethod("tmap_df")
}

#' @export
tmap_df.default <- function(.x, .f, period = "yearly", name = "data", ...) {
  stop("Object is not of class `tbl_time`.", call. = FALSE)
}

#' @export
tmap_df.tbl_time <- function(.x, .f, period = "yearly", name = "data", ...) {
  join_cols <- retrieve_index(.x, as_name = TRUE)
  tmap_variant(.x, .f, period = period, name = name,
               map_type = purrr::map_df, join_cols = join_cols, ...)
}

#' @export
tmap_df.grouped_tbl_time <- function(.x, .f, period = "yearly", name = "data", ...) {
  join_cols <- c(dplyr::group_vars(.x), retrieve_index(.x, as_name = TRUE))
  x <- tmap_variant(.x, .f, period = period, name = name,
                    map_type = purrr::map_df, join_cols = join_cols, ...)
  group_by(x, !!! dplyr::groups(.x))
}

##### Util ---------------------------------------------------------------------

tmap_variant <- function(.x, .f,
                         period = "yearly",
                         map_type = NULL,
                         join_cols = NULL,
                         name = "data", ...) {

  # Get the key column name
  .key <- rlang::sym(name)

  # Collapse to the correct period
  time_collapse(.x, period) %>%

    # Ungroup. nest_cols argument handles them
    ungroup() %>%

    # Nest, ignoring grouping cols and index col
    tidyr::nest(- dplyr::one_of(join_cols), .key = !! .key) %>%

    # Map function to each group's data frame
    dplyr::mutate(!! .key := purrr::map(.x = !! .key, .f = ~map_type(.x, .f)))

}
