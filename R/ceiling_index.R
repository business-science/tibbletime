# Most of the time this uses lubridate::ceiling_date()
ceiling_index <- function(x, unit = "seconds") {
  UseMethod("ceiling_index")
}

ceiling_index.default <- function(x, unit = "seconds") {
  lubridate::ceiling_date(x, unit)
}

ceiling_index.hms <- function(x, unit = "seconds") {
  hms::as.hms(ceiling_index(as.POSIXct(x), unit))
}

ceiling_index.yearmon <- function(x, unit = "seconds") {
  zoo::as.yearmon(ceiling_index(zoo::as.Date(x), unit))
}

ceiling_index.yearqtr <- function(x, unit = "seconds") {
  zoo::as.yearqtr(ceiling_index(zoo::as.Date(x), unit))
}
