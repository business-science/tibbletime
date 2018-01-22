#' A convenient function for cleaning an index column
#'
#' Round the index column of your `tbl_time` object to a specified period.
#' Round down with `side = "start"` and round up with `side = "end"`.
#'
#'
#' @param .tbl_time A `tbl_time` object.
#' @param period The period to use for rounding. Specified like in [collapse_by()].
#' @param side Either "start" to round down or "end" to round up.
#'
#' @details
#'
#' `clean()` uses modified versions of [lubridate::ceiling_date()] and
#' [lubridate::floor_date()] to perform the rounding. They are modified to
#' work with other index classes like `yearmon` and `hms`.
#'
#' `clean()` is most useful after a call to [collapse_by()] but before a
#' [dplyr::group_by()].
#'
#' For lower level use access to the rounding functions, use [ceiling_index()]
#' and [floor_index()] directly.
#'
#' @seealso [ceiling_index()], [floor_index()]
#'
#' @examples
#'
#' data(FB)
#'
#' FB %>%
#'   as_tbl_time(date) %>%
#'   collapse_by("2 month") %>%
#'   clean("month") %>%
#'   dplyr::group_by(date) %>%
#'   dplyr::summarise(mean_adj = mean(adjusted))
#'
#' @export
clean <- function(.tbl_time, period = "yearly", side = "end") {

  index_char <- get_index_char(.tbl_time)
  index_quo  <- get_index_quo(.tbl_time)

  assert_valid_side(side)

  parsed_period <- parse_period(period)
  period        <- paste(parsed_period[["freq"]], parsed_period[["period"]])

  round_fun <- switch (side,
                       "start" = floor_index,
                       "end"   = ceiling_index
  )

  # Slower than directly modifying the column but safer as otherwise you can
  # bypass the "don't modify the group column" feature of mutate
  .tbl_time_cleaned <- dplyr::mutate(.tbl_time, !! index_char := round_fun(!! index_quo, unit = period))

  reconstruct(.tbl_time_cleaned, .tbl_time)
}



#' A simple wrapper of [lubridate::ceiling_date()]
#'
#' This is a thin wrapper around a [lubridate::ceiling_date()] that works
#' for `hms`, `yearmon`, and `yearqtr` classes as well.
#'
#' @inheritParams lubridate::ceiling_date
#'
#' @examples
#'
#' data(FB)
#' dplyr::mutate(FB, date2 = ceiling_index(date, "year"))
#'
#' time_test <- create_series('00:00:00'~'12:00:00',
#'                            '1 minute', class = "hms")
#'
#' dplyr::mutate(time_test, date2 = ceiling_index(date, "hour"))
#'
#' @seealso [lubridate::ceiling_date()]
#'
#' @export
ceiling_index <- function(x, unit = "seconds") {
  UseMethod("ceiling_index")
}

#' @export
ceiling_index.default <- function(x, unit = "seconds") {
  lubridate::ceiling_date(x, unit)
}

#' @export
ceiling_index.hms <- function(x, unit = "seconds") {
  ceilinged <- ceiling_index(as.POSIXct(x), unit)
  hms::as.hms(ceilinged, get_index_col_time_zone(ceilinged))
}

#' @export
ceiling_index.yearmon <- function(x, unit = "seconds") {
  zoo::as.yearmon(ceiling_index(zoo::as.Date(x), unit))
}

#' @export
ceiling_index.yearqtr <- function(x, unit = "seconds") {
  zoo::as.yearqtr(ceiling_index(zoo::as.Date(x), unit))
}



#' A simple wrapper of [lubridate::floor_date()]
#'
#' This is a thin wrapper around a [lubridate::floor_date()] that works
#' for `hms`, `yearmon`, and `yearqtr` classes as well.
#'
#' @inheritParams lubridate::floor_date
#'
#' @examples
#'
#' data(FB)
#' dplyr::mutate(FB, date2 = floor_index(date, "year"))
#'
#' time_test <- create_series('00:00:00'~'12:00:00',
#'                            '1 minute', class = "hms")
#'
#' dplyr::mutate(time_test, date2 = floor_index(date, "hour"))
#'
#' @seealso [lubridate::floor_date()]
#'
#' @export
floor_index <- function(x, unit = "seconds") {
  UseMethod("floor_index")
}

#' @export
floor_index.default <- function(x, unit = "seconds") {
  lubridate::floor_date(x, unit)
}

#' @export
floor_index.hms <- function(x, unit = "seconds") {
  floored <- floor_index(as.POSIXct(x), unit)
  hms::as.hms(floored, attr(floored, "tzone"))
}

#' @export
floor_index.yearmon <- function(x, unit = "seconds") {
  zoo::as.yearmon(floor_index(zoo::as.Date(x), unit))
}

#' @export
floor_index.yearqtr <- function(x, unit = "seconds") {
  zoo::as.yearqtr(floor_index(zoo::as.Date(x), unit))
}

