# both must be tbl_time

time_left_join <- function(x, y, by = NULL, period = "yearly", copy = FALSE, suffix = c(".x", ".y"), ...) {
  
  if(get_index_char(x) %in% by || get_index_char(y) %in% by) {
    stop("Do not specify the time index in `by`. Use `period` instead.")
  }
  
  x <- time_group(x, period = period)
  y <- time_group(y, period = period)
  
  y <- y %>% select(- !! get_index_quo(y))
  
  left_join(x, y, by = c(by, ".time_group"), copy = copy, suffix = suffix, ...) %>%
    select(-.time_group)
}

time_right_join <- function(x, y, by = NULL, period = "yearly", copy = FALSE, suffix = c(".x", ".y"), ...) {
  
  x <- time_group(x, period = period)
  y <- time_group(y, period = period)
  
  x <- x %>% select(- !! get_index_quo(x))
  
  right_join(x, y, by = c(by, ".time_group")) %>%
    select(-.time_group)
}
