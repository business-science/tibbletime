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

  # Verify period
  period <- verify_period(period)

  # Index name as sym
  index_name <- rlang::sym(retrieve_index(x, as_name = TRUE))

  # Split by ':' into separate lists
  from_to <- strsplit(period, ",") %>%
    unlist()
  from_to <- list(from = from_to[1],
                  to   = from_to[2])

  # Normalize
  from_to_clean <- purrr::map2_chr(from_to, c("from", "to"), .f = normalize_date)

  # Filter for those rows
  dplyr::filter(x,
                UQ(index_name) >= as.Date(from_to_clean[["from"]]),
                UQ(index_name) <= as.Date(from_to_clean[["to"]]))

}


# Util ----

verify_period <- function(period) {

  split_count <- stringr::str_count(period, ",")

  assertthat::assert_that(split_count %in% c(0, 1) ,
                          msg = "Only a `from` and `to` period can be supplied")

  if(split_count == 0) {
    period <- paste(period, period, sep = ",")
  }

  period
}

normalize_date <- function(x, from_to) {

  # Setup list of filters to fill
  filters <- switch (from_to,
    "from" = list(year = "1970", month = "01", day = "01"),
    "to"   = list(year = "1970", month = "12", day = "31")
  )

  # Split filter date by "-"
  x <- stringr::str_split(x, "-") %>%
    unlist()

  # Fill the list as far as it goes
  # Defaults are left for unfilled list pieces
  for(i in 1:length(x)) {
    filters[[i]] <- x[i]
  }

  # Unroll list and collapse into Date formatted character
  unlist(filters) %>%
    paste0(collapse = "-")
}
