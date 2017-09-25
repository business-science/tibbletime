#' @importFrom magrittr %>%
#' @export
#'
magrittr::`%>%`

#' @importFrom rlang :=
#'
NULL

# Convert a period to an expanded list of syms required to expand the index
period_to_syms <- function(period) {
  groups <- switch (period,
      "yearly"     = list("year"),
      "quarterly"  = list("year", "quarter"),
      "monthly"    = list("year", "month"),
      "weekly"     = list("year", "month", "week"),
      "daily"      = list("year", "month", "day"),
      "hourly"     = list("year", "month", "day", "hour"),
      "minute"     = list("year", "month", "day", "hour", "minute"),
      "second"     = list("year", "month", "day", "hour", "minute", "second"),
      stop("`period` is not one of: 'yearly', 'quarterly', 'monthly', 'weekly',
           'daily', 'hourly', 'minute', 'second'", call. = FALSE)
  )

  rlang::syms(groups)
}


