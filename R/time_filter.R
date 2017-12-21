#' Succinctly filter a `tbl_time` object by its index
#'
#' Use a concise filtering method to filter a `tbl_time` object by its `index`.
#'
#' @param x A `tbl_time` object.
#' @param time_formula A period to filter over.
#' This is specified as a `formula`. See `Details`.
#'
#' @details
#'
#' The `time_formula` is specified using the format `from ~ to`.
#' Each side of the `time_formula` is specified as the character
#' `'YYYY-MM-DD HH:MM:SS'`, but powerful shorthand is available.
#' Some examples are:
#' * __Year:__ `'2013' ~ '2015'`
#' * __Month:__ `'2013-01' ~ '2016-06'`
#' * __Day:__ `'2013-01-05' ~ '2016-06-04'`
#' * __Second:__ `'2013-01-05 10:22:15' ~ '2018-06-03 12:14:22'`
#' * __Variations:__ `'2013' ~ '2016-06'`
#'
#' The `time_formula` can also use a one sided formula.
#' * __Only dates in 2015:__ `~'2015'`
#' * __Only dates March 2015:__ `~'2015-03'`
#'
#' The `time_formula` can also use `'start'` and `'end'` as keywords for
#' your filter.
#' * __Start of the series to end of 2015:__ `'start' ~ '2015'`
#' * __Start of 2014 to end of series:__ `'2014' ~ 'end'`
#'
#' All shorthand dates are expanded:
#' * The `from` side is expanded to be the first date in that period
#' * The `to` side is expanded to be the last date in that period
#'
#' This means that the following examples are equivalent (assuming your
#' index is a POSIXct):
#' * `2015 ~ 2016 == 2015-01-01 + 00:00:00 ~ 2016-12-31 + 23:59:59`
#' * `~2015 == 2015-01-01 + 00:00:00 ~ 2015-12-31 + 23:59:59`
#' * `2015-01-04 + 10:12 ~ 2015-01-05 == 2015-01-04 + 10:12:00 ~ 2015-01-05 + 23:59:59`
#'
#' This function respects [dplyr::group_by()] groups.
#'
#' @rdname time_filter
#'
#' @export
#'
#' @examples
#'
#' # FANG contains Facebook, Amazon, Netflix and Google stock prices
#' data(FANG)
#' FANG <- as_tbl_time(FANG, date) %>%
#'   dplyr::group_by(symbol)
#'
#' # 2013-01-01 to 2014-12-31
#' time_filter(FANG, '2013' ~ '2014')
#'
#' # 2013-05-25 to 2014-06-04
#' time_filter(FANG, '2013-05-25' ~ '2014-06-04')
#'
#' # Using the `[` subset operator
#' FANG['2014'~'2015']
#'
#' # Using `[` and one sided formula for only dates in 2014
#' FANG[~'2014']
#'
#' # Using `[` and column selection
#' FANG['2013'~'2016', c("date", "adjusted")]
#'
#' # Variables are unquoted using rlang
#' lhs_date <- "2013"
#' rhs_date <- as.Date("2014-01-01")
#' time_filter(FANG, lhs_date ~ rhs_date)
#'
#' # Use the keywords 'start' and 'end' to conveniently access ends
#' time_filter(FANG, 'start' ~ '2014')
#'
#'
time_filter <- function(x, time_formula) {
  UseMethod("time_filter")
}

#' @export
time_filter.default <- function(x, time_formula) {
  stop("Object is not of class `tbl_time`.", call. = FALSE)
}

#' @export
time_filter.tbl_time <- function(x, time_formula) {

  index_quo  <- get_index_quo(x)
  tz <- get_index_time_zone(x)

  # from/to setup is done inside the call to filter so it is unique to
  # each group
  x_filtered <- dplyr::filter(x, {

    # Parse the time_formula, don't convert to dates yet
    tf_list <- parse_time_formula(!! index_quo, time_formula)

    # Could allow for multifilter idea here

    # Then convert to datetime
    from_to <- purrr::map(
      .x = tf_list,
      .f = ~list_to_datetime(!! index_quo, .x, tz = tz)
    )

    # Get sequence creation pieces ready
    from <- from_to[[1]]
    to   <- from_to[[2]]

    # Final assertion of order
    assert_from_before_to(from, to)

    sorted_range_search(!! index_quo, from, to)
  })

  sloop::reconstruct(x_filtered, x)
}

# Subset operator --------------------------------------------------------------

#' @export
#'
#' @param i A period to filter over. This is specified the same as
#' `time_formula` or can use the traditional row extraction method.
#' @param j Optional argument to also specify column index to subset. Works
#' exactly like the normal extraction operator.
#' @param drop Will always be coerced to `FALSE` by `tibble`.
#'
#' @rdname time_filter
#'
`[.tbl_time` <- function(x, i, j, drop = FALSE) {

  # This helps decide whether i is used for column subset or row subset
  .nargs <- nargs() - !missing(drop)

  # time_filter if required
  if(!missing(i)) {
    if(rlang::is_formula(i)) {
      x <- time_filter(x, i)
    }
  }

  # Remove time class/attribs to let tibble::`[` do the rest
  x_tbl <- as_tibble(x)

  # i filter
  if(!missing(i)) {
    if(!rlang::is_formula(i)) {
      if(.nargs <= 2) {
        # Column subset
        # Preferred if tibble issue is addressed
        # x <- x[i, drop = drop]
        x_tbl <- x_tbl[i]
      } else {
        # Row subset
        x_tbl <- x_tbl[i, , drop = drop]
      }

    }
  }

  # j filter
  if(!missing(j)) {
    x_tbl <- x_tbl[, j, drop = drop]
  }

  # If the index still exists, convert to tbl_time again
  if(get_index_char(x) %in% colnames(x_tbl)) {
    x_tbl <- as_tbl_time(x_tbl, !! get_index_quo(x))
  }

  x_tbl
}

#' @export
# `[.grouped_tbl_time` <- function(x, i, j, drop = FALSE) {
#   x_tbl <- NextMethod()
#
#   group_names <- dplyr::group_vars(x)
#
#   # If the groups have been removed
#   if(!all(group_names %in% colnames(x_tbl))) {
#
#     if(inherits(x_tbl, "tbl_time")) {
#
#       as_tbl_time(x_tbl, !! get_index_quo(x_tbl))
#
#     } else {
#
#       as_tibble(x_tbl)
#
#     }
#
#     # If all the groups are still there
#   } else {
#
#     if(inherits(x_tbl, "tbl_time")) {
#
#       grouped_tbl_time(dplyr::grouped_df(x_tbl, group_names), !! get_index_quo(x_tbl))
#
#     } else {
#
#       dplyr::grouped_df(x_tbl, group_names)
#
#     }
#
#   }
# }

