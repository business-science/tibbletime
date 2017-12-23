# These are simple, and validation is done outside the function so that only
# unique Dates -> yearmon are created. E.g., from/to will only end up being 1st of
# the month and `by` will be year + month / year + quarter

seq.yearmon <- function(from, to, by, ...) {
  .seq <- seq.Date(
    zoo::as.Date(from, tz = get_default_time_zone()),
    zoo::as.Date(to, tz = get_default_time_zone()),
    by
  )
  zoo::as.yearmon(.seq)
}

seq.yearqtr <- function(from, to, by, ...) {
  .seq <- seq.Date(
    zoo::as.Date(from, tz = get_default_time_zone()),
    zoo::as.Date(to, tz = get_default_time_zone()),
    by
  )
  zoo::as.yearqtr(.seq)
}

seq.hms <- function(from, to, by, ...) {
  .seq <- seq.POSIXt(
    as.POSIXct(from),
    as.POSIXct(to),
    by
  )
  hms::as.hms(.seq)
}
