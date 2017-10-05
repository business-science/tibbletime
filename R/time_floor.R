#' Floor a `tbl_time` object
#'
#' This is essentially a convenient wrapper to [lubridate::floor_date()] that
#' allows flooring of a `tbl_time` object using a period formula.
#'
#' @inheritParams time_group
#' @param x A `tbl_time` object.
#' @param ... Arguments passed on to [lubridate::floor_date()]
#'
#' @details
#'
#' This function respects [dplyr::group_by()] groups.
#'
#' @examples
#'
#' example_series <- create_series(~2013, 2~day)
#'
#' # When you convert to a period, the lowest available date in that period
#' # is chosen
#' example_series %>%
#'   as_period(1~m)
#'
#' # Sometimes you want to additionally floor that date so that all dates
#' # are consistent
#' example_series %>%
#'   as_period(1~m) %>%
#'   time_floor(1~m)
#'
#' @export
time_floor <- function(x, period, ...) {
  UseMethod("time_floor")
}

#' @export
time_floor.default <- function(x, period, ...) {
  stop("Object is not of class `tbl_time`.", call. = FALSE)
}

#' @export
time_floor.tbl_time <- function(x, period, ...) {

  # Period and index
  period <- split_period(period)
  index <- rlang::sym(retrieve_index(x, as_name = TRUE))

  # Unit for lubridate
  lub_unit <- paste(period[["num"]], period[["period"]])

  # Apply floor
  mutate(x,
         !! index := lubridate::floor_date(!! index, unit = lub_unit, ...))

}
