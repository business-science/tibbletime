# Most of the time this uses lubridate::floor_date()
floor_index <- function(x, unit = "seconds") {
  UseMethod("floor_index")
}

floor_index.default <- function(x, unit = "seconds") {
  lubridate::floor_date(x, unit)
}

floor_index.hms <- function(x, unit = "seconds") {
  hms::as.hms(floor_index(as.POSIXct(x), unit))
}

floor_index.yearmon <- function(x, unit = "seconds") {
  zoo::as.yearmon(floor_index(zoo::as.Date(x), unit))
}

floor_index.yearqtr <- function(x, unit = "seconds") {
  zoo::as.yearqtr(floor_index(zoo::as.Date(x), unit))
}
