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
  hms::as.hms(ceiling_index(as.POSIXct(x), unit))
}

#' @export
ceiling_index.yearmon <- function(x, unit = "seconds") {
  zoo::as.yearmon(ceiling_index(zoo::as.Date(x), unit))
}

#' @export
ceiling_index.yearqtr <- function(x, unit = "seconds") {
  zoo::as.yearqtr(ceiling_index(zoo::as.Date(x), unit))
}
