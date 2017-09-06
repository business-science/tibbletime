#' Change `tbl_time` periodicity
#'
#' Convert a `tbl_time` object from daily to monthly,
#' from minute data to hourly, and more. This allows the user to easily
#' aggregate data to a less granular level.
#'
#' @param x A `tbl_time` object.
#' @param period The period to convert to. By default, yearly.
#' @param side Whether to return the date at the beginning or the end of the
#' new period. By default, the `"start"` of the period. Use `"end"` to change
#' to the end of the period.
#'
#' @details
#'
#' This function respects [dplyr::group_by()] groups.
#'
#'
#' Currently periods finer than second data are not supported.
#'
#' The `side` argument is useful when you want to return data at, say, the
#' end of a quarter, or the end of a month.
#'
#' @note
#'
#' The following periods are available:
#' * `"yearly"`
#' * `"quarterly"`
#' * `"monthly"`
#' * `"weekly"`
#' * `"daily"`
#' * `"hour"`
#' * `"minute"`
#' * `"second"`
#'
#' @export
#'
#' @examples
#'
#' # Basic usage ---------------------------------------------------------------
#'
#' # FB stock prices
#' data(FB)
#' FB <- as_tbl_time(FB, date)
#'
#' # Aggregate FB to yearly data
#' as_period(FB, "yearly")
#'
#' # Aggregate FB to yearly data, but use the last data point available
#' # in that period
#' as_period(FB, "yearly", "end")
#'
#' # Aggregate to weekly. Notice that it only uses the earliest day available
#' # in the data set at that periodicity. It will not set the date of the first
#' # row to 2013-01-01 because that date did not exist in the original data set.
#' as_period(FB, "weekly")
#'
#' # FB is daily data, aggregate to minute?
#' # Does nothing and returns the original data set.
#' as_period(FB, "minute")
#'
#' # Grouped usage -------------------------------------------------------------
#'
#' # FANG contains Facebook, Amazon, Netflix and Google stock prices
#' data(FANG)
#' FANG <- as_tbl_time(FANG, date)
#'
#' FANG <- FANG %>% group_by(symbol)
#'
#' # Respects groups
#' FANG %>% as_period("yearly")
#'
as_period <- function(x, period = "yearly", side = "start") {
  UseMethod("as_period")
}

#' @export
as_period.default <- function(x, period = "yearly", side = "start") {
  stop("Object is not of class `tbl_time`.", call. = FALSE)
}

#' @export
as_period.tbl_time <- function(x, period = "yearly", side = "start") {

  # Index tibble/sym
  index      <- expand_index(x, period)
  index_name <- rlang::sym(retrieve_index(x, as_name = TRUE))

  # Define grouping symbols
  groups <- period_to_syms(period)

  # Beginning or end of period?
  side_fun <- switch (side,
    "start" = min,
    "end"   = max
  )

  # Change periodicity of the index alone
  new_dates <- index %>%
    dplyr::group_by(!!! groups, add = TRUE) %>%
    dplyr::filter(rlang::UQ(index_name) == side_fun(!! index_name))

  # Filter the entire data based on the new index
  # Then remove duplicate times (i.e. if multiple trades at 15:00:01, only the first is returned)
  by_vars <- intersect(colnames(x), colnames(index))
  dplyr::semi_join(x, new_dates, by = by_vars) %>%
    dplyr::distinct(!! index_name, .keep_all = TRUE)

}

# Utils ----

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
          stop("`period` is not one of: 'yearly', 'quarterly', 'monthly', 'weekly', 'daily', 'hourly', 'minute', 'second'", call. = FALSE)
  )

  rlang::syms(groups)
}
