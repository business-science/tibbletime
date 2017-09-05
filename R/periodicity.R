#' @export
as_period <- function(x, period = "yearly", side = "start") {

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
    dplyr::filter(UQ(index_name) == side_fun(!! index_name))

  # Filter the entire data based on the new index
  # Then remove duplicate times (i.e. if multiple trades at 15:00:01, only the first is returned)
  by_vars <- intersect(colnames(x), colnames(index))
  dplyr::semi_join(x, new_dates, by = by_vars) %>%
    dplyr::distinct(!! index_name, .keep_all = TRUE)

}

# Utils ----

period_to_syms <- function(period) {
  groups <- switch (period,
          "yearly"  = list("year"),
          "monthly" = list("year", "month"),
          "daily"   = list("year", "month", "day"),
          "hourly"  = list("year", "month", "day", "hour"),
          "minute"  = list("year", "month", "day", "hour", "minute"),
          "second"  = list("year", "month", "day", "hour", "minute", "second"),
          stop("`period` is not one of: 'yearly', 'monthly', 'daily', 'hourly', 'minute', 'second'", call. = FALSE)
  )

  rlang::syms(groups)
}
