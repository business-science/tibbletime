#' Succinctly filter a `tbl_time` object by date
#'
#' Use a concise filtering method to filter for rows where the `index` falls within a date range
#'
#' @details
#'
#' Filtering is specified using the format `"from,to"` where the `from` and `to` are specified as:
#' * Year - "2013,2015"
#' * Month - "2013-01,2016-06"
#' * Day - "2013-01-05,2016-06-04"
#' * Variations - "2013,2016-06"
#'
#' @param x stuff!!
#' @param period stuff!!
#'
#' @export
#'
#' @examples
#'
#' examples!!
#'
time_filter <- function(x, period) {

  # Validate period syntax
  from_to <- validate_period(period)

  # Index name as sym
  index_name <- rlang::sym(retrieve_index(x, as_name = TRUE))

  # Normalize
  from_to_clean <- purrr::map2_chr(from_to, c("from", "to"), .f = normalize_date)

  # Validate period date order
  validate_date_order(from = from_to_clean[["from"]], to = from_to_clean[["to"]])

  # Filter for those rows
  dplyr::filter(x,
                UQ(index_name) >= as.POSIXct(from_to_clean[["from"]], tz = retrieve_time_zone(x)),
                UQ(index_name) <= as.POSIXct(from_to_clean[["to"]],   tz = retrieve_time_zone(x)))

}


# Util ----

# Check a user supplied period for correct syntax
# If "2015", duplicate to "2015,2015"
# Removes leading / trailing spaces
validate_period <- function(period) {

  # Comma separation
  split_count <- stringr::str_count(period, ",")

  assertthat::assert_that(split_count %in% c(0, 1) ,
                          msg = "Only a `from` and `to` period can be supplied, separated by a comma")

  if(split_count == 0) {
    period <- paste(period, period, sep = ",")
  }

  # Split by ',' into separate lists
  from_to <- unlist(strsplit(period, ","))
  from_to <- list(from = from_to[1], to = from_to[2])

  # Leading and trailing spaces
  from_to <- lapply(from_to, FUN = function(x) gsub("^\\s+|\\s+$", "", x))

  # Check symbols
  check_syms <- function(x) {
    assertthat::assert_that(stringr::str_count(x, "-") <= 2,
                            msg = "A maximum of three '-' are possible per date")
    assertthat::assert_that(stringr::str_count(x, ":") <= 2,
                            msg = "A maximum of three ':' are possible per date")
    assertthat::assert_that(stringr::str_count(x, " ") <= 1,
                            msg = "A maximum of one space is possible per date")
    assertthat::assert_that(!stringr::str_detect(x, "^:|:$|\\s:|:\\s"),
                            msg = "A ':' can only be used between two numbers")
    assertthat::assert_that(!stringr::str_detect(x, "^-|-$|\\s-|-\\s"),
                            msg = "A '-' can only be used between two numbers")
  }
  lapply(from_to, check_syms)

  from_to
}

# Validates the final dates
# `from` must be before `to`
validate_date_order <- function(from, to) {
  from <- as.POSIXct(from)
  to   <- as.POSIXct(to)

  assertthat::assert_that(from <= to, msg = "`from` must be a date before `to`")
}

# Expand date shorthand into a real date
normalize_date <- function(x, from_to) {

  # Setup ymd / hms lists to fill
  ymd <- switch(from_to,
                "from" = list(y = "1970", m = "01", d = "01"),
                "to"   = list(y = "1970", m = "12", d = "00")) # `to` day depends on month selected, filled later if necessary

  hms <- switch(from_to,
                "from" = list(h = "00", m = "00", s = "00"),
                "to"   = list(h = "23", m = "59", s = "59"))

  # Check existance of date / time dividing space \\s
  date_time <- if(stringr::str_detect(x, "\\s")) {

    # If there is a date and a time, split them
    date_time <- unlist(stringr::str_split(x, "\\s"))

    # Recurse split to fill the lists
    date <- recurse_split(date_time[1], ymd, "-")
    time <- recurse_split(date_time[2], hms, ":")

    # Paste together
    paste(date, time, sep = " ")
  } else {

    # If there is only a date, no time
    # Recurse split the date, and pass 0 as time
    date <- recurse_split(x, ymd, "-")
    time <- recurse_split("00:00:00", hms, ":")

    # Paste together
    paste(date, time, sep = " ")
  }

  date_time
}

# Split x by the splitter and fill the filler list with the pieces
recurse_split <- function(x, filler, splitter) {
  i <- 1

  # While there is something to split on
  while(stringr::str_detect(x, splitter)) {

    # Extract the first part of the split
    piece <- stringr::str_extract(x, paste0("([^", splitter, "]+)"))

    # Replace the first part with "" in the string
    x <- stringr::str_replace(x, pattern = paste0("([^", splitter, "]+", splitter, ")"), replacement = "")

    # Add the new piece to the filler
    filler[[i]] <- piece

    # Next
    i <- i + 1
  }
  # Once there is nothing left to split on, add the last piece to the filler
  filler[[i]] <- x

  # If `to` d was never changed, set as end of chosen month
  if(!is.null(filler[["d"]])) {
    if(filler[["d"]] == "00") {

      # Fake a date to find the number of days in that month
      filler[["d"]] <- "01"
      fake_date <- as.Date(paste0(unlist(filler), collapse = splitter))

      # Fill the `to` d with the number of days in that month
      filler[["d"]] <- as.character(lubridate::days_in_month(fake_date))
    }
  }

  paste0(unlist(filler), collapse = splitter)
}
