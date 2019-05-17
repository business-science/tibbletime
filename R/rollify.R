#' Create a rolling version of any function
#'
#' `rollify` returns a rolling version of the input function, with a
#' rolling `window` specified by the user.
#'
#' @inheritParams purrr::quietly
#' @param window The window size to roll over
#' @param unlist If the function returns a single value each time it is called,
#' use `unlist = TRUE`. If the function returns more than one value, or a more
#' complicated object (like a linear model), use `unlist = FALSE` to create
#' a list-column of the rolling results.
#' @param na_value A default value for the `NA` values at the beginning of the
#' roll.
#' @param expand Calculate for rolling 8default) or expanding window.
#'
#' @details
#'
#' The intended use of `rollify` is to turn a function into a rolling version
#' of itself for use inside of a call to [dplyr::mutate()], however it works
#' equally as well when called from [purrr::map()].
#'
#' Because of it's intended use with [dplyr::mutate()], `rollify`
#' creates a function that always returns output with the same length of the
#' input, aligned right, and filled with `NA` unless otherwise specified
#' by `na_value`.
#'
#' The form of the `.f` argument is the same as the form that can be passed
#' to [purrr::map()]. Use `.x` or `.` to refer to the first object to roll over,
#' and `.y` to refer to the second object if required. The examples explain this
#' further.
#'
#' If optional arguments to the function are required, specify them in the
#' call to `rollify`, and not in the call to the rolling version of the
#' function. See the examples for more details.
#'
#'
#' @examples
#'
#' # Rolling mean --------------------------------------------------------------
#'
#' data(FB)
#'
#' # Turn the normal mean function into a rolling mean with a 5 row window
#' mean_roll_5 <- rollify(mean, window = 5)
#'
#' dplyr::mutate(FB,
#'        normal_mean  = mean(adjusted),
#'        rolling_mean = mean_roll_5(adjusted))
#'
#' # Turn the normal mean function into an expanding mean starting with a 5 row window
#' mean_roll_5e <- rollify(mean, window = 5, expand = TRUE)
#'
#' dplyr::mutate(FB,
#'        normal_mean  = mean(adjusted),
#'        rolling_mean = mean_roll_5e(adjusted))
#'
#' # There's nothing stopping you from combining multiple rolling functions with
#' # different window sizes in the same mutate call
#' mean_roll_10 <- rollify(mean, window = 10)
#'
#' dplyr::mutate(FB,
#'        rolling_mean_5  = mean_roll_5(adjusted),
#'        rolling_mean_5e  = mean_roll_5e(adjusted),
#'        rolling_mean_10 = mean_roll_10(adjusted))
#'
#' # Functions with multiple args and optional args ----------------------------
#'
#' # With 2 args, use the purrr syntax of ~ and .x, .y
#' # Rolling correlation example
#' cor_roll <- rollify(~cor(.x, .y), window = 5)
#'
#' dplyr::mutate(FB, running_cor = cor_roll(adjusted, open))
#'
#' # With >2 args, create an anonymous function with >2 args or use
#' # the purrr convention of ..1, ..2, ..3 to refer to the arguments
#' avg_of_avgs <- rollify(function(x, y, z) {
#'                          (mean(x) + mean(y) + mean(z)) / 3
#'                        },
#'                        window = 10)
#'
#' # Or
#' avg_of_avgs <- rollify(~(mean(..1) + mean(..2) + mean(..3)) / 3,
#'                        window = 10)
#'
#' dplyr::mutate(FB, avg_of_avgs = avg_of_avgs(open, high, low))
#'
#' # Optional arguments MUST be passed at the creation of the rolling function
#' # Only data arguments that are "rolled over" are allowed when calling the
#' # rolling version of the function
#' FB$adjusted[1] <- NA
#'
#' roll_mean_na_rm <- rollify(~mean(.x, na.rm = TRUE), window = 5)
#'
#' dplyr::mutate(FB, roll_mean = roll_mean_na_rm(adjusted))
#'
#' # Returning multiple values -------------------------------------------------
#'
#' data(FB)
#'
#' # If the function returns >1 value, set the `unlist = FALSE` argument
#' # Running 5 number summary
#' summary_roll <- rollify(summary, window = 5, unlist = FALSE)
#'
#' FB_summarised <- dplyr::mutate(FB, summary_roll = summary_roll(adjusted))
#' FB_summarised$summary_roll[[5]]
#'
#' # dplyr::bind_rows() is often helpful in these cases to get
#' # meaningful output
#'
#' summary_roll <- rollify(~dplyr::bind_rows(summary(.)), window = 5, unlist = FALSE)
#' FB_summarised <- dplyr::mutate(FB, summary_roll = summary_roll(adjusted))
#' FB_summarised %>%
#'   dplyr::filter(!is.na(summary_roll)) %>%
#'   tidyr::unnest(summary_roll)
#'
#' # Rolling regressions -------------------------------------------------------
#'
#' # Extending an example from R 4 Data Science on "Many Models".
#' # For each country in the gapminder data, calculate a linear regression
#' # every 5 periods of lifeExp ~ year
#' library(gapminder)
#'
#' # Rolling regressions are easy to implement
#' lm_roll <- rollify(~lm(.x ~ .y), window = 5, unlist = FALSE)
#'
#' gapminder %>%
#'   dplyr::group_by(country) %>%
#'   dplyr::mutate(rolling_lm = lm_roll(lifeExp, year))
#'
#' # Rolling with groups -------------------------------------------------------
#'
#' # One of the most powerful things about this is that it works with
#' # groups since `mutate` is being used
#' data(FANG)
#' FANG <- FANG %>%
#'   dplyr::group_by(symbol)
#'
#' mean_roll_3 <- rollify(mean, window = 3)
#'
#' FANG %>%
#'   dplyr::mutate(mean_roll = mean_roll_3(adjusted)) %>%
#'   dplyr::slice(1:5)
#'
#' @seealso [purrr::safely], [purrr::possibly]
#'
#' @export
#'
rollify <- function(.f, window = 1, unlist = TRUE, na_value = NULL, expand = FALSE) {

  # Mappify the function
  .f <- purrr::as_mapper(.f)

  # Return function that calls roller
  function(...) {
    roller(..., .f = .f, window = window, unlist = unlist, na_value = na_value, expand = expand)
  }
}


# Utils ------------------------------------------------------------------------

roller <- function(..., .f, window, unlist = TRUE, na_value = NULL, expand = FALSE) {

  # na_value as NA if not specified
  if(is.null(na_value)) {
    na_value = NA
  }

  # Capture dots as list. These should be the arguments that are rolled
  .dots <- rlang::dots_list(...)

  # Error check the dots
  check_dots(.dots, window)

  # Each data element of .dots should be of the same length so use the first
  # as the length of the dataset
  roll_length <- length(.dots[[1]])

  # Initialize `filled` vector
  filled <- rlang::rep_along(1:roll_length, list(na_value))

  # Roll and fill
  if (expand) {
    for(i in window:roll_length) {
      .f_dots   <- lapply(.dots, function(x) {x[1:i]})
      filled[[i]] <- do.call(.f, .f_dots)
    }
  } else {
    for(i in window:roll_length) {
      .f_dots   <- lapply(.dots, function(x) {x[(i-window+1):i]})
      filled[[i]] <- do.call(.f, .f_dots)
    }
  }

  # Don't unlist if requested (when >1 value returned)
  if(unlist) {
    unlist(filled)
  } else {
    filled
  }

}

# Check that dots follow the necessary convention for rolling
check_dots <- function(x, window) {

  # The user must have passed something to be passed on to .f
  assertthat::assert_that(length(x) > 0,
                          msg = "At least 1 data argument must be supplied to be
                          passed on to the rolling function")


  # The window must be smaller than the length of the data
  assertthat::assert_that(window <= length(x[[1]]),
                          msg = "Cannot roll apply with a window larger than the
                          length of the data")


  # Length of every element of .dots should be the same
  # Only data used in the rolling should be in .dots
  # Optional args should be specified in the rollify call
  for(i in 1:length(x)) {
    assertthat::assert_that(length(x[[i]]) == length(x[[1]]),
                            msg = "Arguments supplied to the rolling version
                            of the function should be data of the same length.
                            Optional arguments should be specified when creating
                            the rolling version with `rollify()`")
  }
}
