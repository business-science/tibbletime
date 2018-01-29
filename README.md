
<!-- README.md is generated from README.Rmd. Please edit that file -->
tibbletime <img src="man/figures/tibbletime-logo.png" width="147" height="170" align="right" />
===============================================================================================

[![Travis-CI Build Status](https://travis-ci.org/business-science/tibbletime.svg?branch=master)](https://travis-ci.org/business-science/tibbletime.svg?branch=master) [![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/tibbletime)](https://cran.r-project.org/package=tibbletime) [![codecov](https://codecov.io/gh/business-science/tibbletime/branch/master/graph/badge.svg)](https://codecov.io/gh/business-science/tibbletime)

A time aware tibble
-------------------

Built on top of the `tidyverse`, `tibbletime` is an extension that allows for the creation of *time aware tibbles* through the setting of a time index.

Some immediate advantages of this include:

1.  Performing compact time-based subsetting on tibbles.

2.  Partitioning an index column by time (like yearly, monthly, every 2 weeks, etc.) so that you can use `dplyr`'s grouped functionality to summarise and aggregate by time period.

3.  Changing the periodicity of a time-based tibble. This allows easily changing from a daily dataset to a monthly or yearly dataset.

4.  Easily working with the pipe and packages like `dplyr` and `tidyr` to make for a seamless experience with time series and the tidyverse. Each function has also been designed to work with `dplyr::group_by()` allowing for powerful data manipulation.

5.  Modifying functions for rolling analysis.

6.  Creating `tbl_time` time series objects quickly.

7.  Using fully supported `Date` and `POSIXct` index columns, along with experimental support for `yearmon`, `yearqtr` and `hms` which should become more stable as some issues in `dplyr` are worked out.

Installation
------------

Development Version:

``` r
# install.packages("devtools")
devtools::install_github("business-science/tibbletime")
```

CRAN Version:

``` r
install.packages("tibbletime")
```

Major update warning
--------------------

If you have been using `0.0.2`, the update to `0.1.0` has introduced major breaking changes. This was necessary for long term stability of the package, and no attempt to support backwards compatability was made at this early stage in development. We apologize for any issues this causes. See NEWS for complete details.

Getting started
---------------

The first thing to do is to turn your `tibble` into a `tbl_time` object. Notice the specification of the `index` as the `date` column of `FB`.

``` r
library(tibbletime)
library(dplyr)

# Facebook stock prices. Comes with the package
data(FB)

# Convert FB to tbl_time
FB <- as_tbl_time(FB, index = date)

FB
#> # A time tibble: 1,008 x 8
#> # Index: date
#>    symbol date        open  high   low close    volume adjusted
#>    <chr>  <date>     <dbl> <dbl> <dbl> <dbl>     <dbl>    <dbl>
#>  1 FB     2013-01-02  27.4  28.2  27.4  28.0  69846400     28.0
#>  2 FB     2013-01-03  27.9  28.5  27.6  27.8  63140600     27.8
#>  3 FB     2013-01-04  28.0  28.9  27.8  28.8  72715400     28.8
#>  4 FB     2013-01-07  28.7  29.8  28.6  29.4  83781800     29.4
#>  5 FB     2013-01-08  29.5  29.6  28.9  29.1  45871300     29.1
#>  6 FB     2013-01-09  29.7  30.6  29.5  30.6 104787700     30.6
#>  7 FB     2013-01-10  30.6  31.5  30.3  31.3  95316400     31.3
#>  8 FB     2013-01-11  31.3  32.0  31.1  31.7  89598000     31.7
#>  9 FB     2013-01-14  32.1  32.2  30.6  31.0  98892800     31.0
#> 10 FB     2013-01-15  30.6  31.7  29.9  30.1 173242600     30.1
#> # ... with 998 more rows
```

There are a number of functions that were designed specifically for `tbl_time` objects. Some of them are:

1.  `filter_time()` - Succinctly filter a tbl\_time object by date.

2.  `as_period()` - Convert a tbl\_time object from daily to monthly, from minute data to hourly, and more. This allows the user to easily aggregate data to a less granular level.

3.  `collapse_by()` - Take an `tbl_time` object, and collapse the index so that all observations in an interval share the same date. The most common use of this is to then group on this column with `dplyr::group_by()` and perform time-based calculations with `summarise()`, `mutate()` or any other `dplyr` function.

4.  `collapse_index()` - A lower level version of `collapse_by()` that directly modifies the `index` column and not the entire `tbl_time` object. It allows the user more flexibility when collapsing, like the ability to assign the resulting collapsed index to a new column.

5.  `rollify()` - Modify a function so that it calculates a value (or a set of values) at specific time intervals. This can be used for rolling averages and other rolling calculations inside the `tidyverse` framework.

6.  `create_series()` - Use shorthand notation to quickly initialize a `tbl_time` object containing a regularly spaced index column of class `Date`, `POSIXct`, `yearmon`, `yearqtr` or `hms`.

To look at just a few:

``` r
# Filter for dates from March 2013 to December 2015
FB %>% 
  filter_time('2013-03' ~ '2015')
#> # A time tibble: 716 x 8
#> # Index: date
#>    symbol date        open  high   low close   volume adjusted
#>    <chr>  <date>     <dbl> <dbl> <dbl> <dbl>    <dbl>    <dbl>
#>  1 FB     2013-03-01  27.0  28.1  26.8  27.8 54064800     27.8
#>  2 FB     2013-03-04  27.8  28.1  27.4  27.7 32400700     27.7
#>  3 FB     2013-03-05  27.9  28.2  27.2  27.5 40622200     27.5
#>  4 FB     2013-03-06  28.1  28.1  27.4  27.5 33532600     27.5
#>  5 FB     2013-03-07  27.6  28.7  27.5  28.6 74540200     28.6
#>  6 FB     2013-03-08  28.4  28.5  27.7  28.0 44198900     28.0
#>  7 FB     2013-03-11  28.0  28.6  27.8  28.1 35642100     28.1
#>  8 FB     2013-03-12  28.1  28.3  27.6  27.8 27569600     27.8
#>  9 FB     2013-03-13  27.6  27.6  26.9  27.1 39619500     27.1
#> 10 FB     2013-03-14  27.1  27.4  26.8  27.0 27646400     27.0
#> # ... with 706 more rows

# Change from daily to monthly periodicity
# This just reduces the tibble to the last row in each month
FB %>% 
  as_period("monthly", side = "end")
#> # A time tibble: 48 x 8
#> # Index: date
#>    symbol date        open  high   low close    volume adjusted
#>    <chr>  <date>     <dbl> <dbl> <dbl> <dbl>     <dbl>    <dbl>
#>  1 FB     2013-01-31  29.2  31.5  28.7  31.0 190744900     31.0
#>  2 FB     2013-02-28  26.8  27.3  26.3  27.2  83027800     27.2
#>  3 FB     2013-03-28  26.1  26.2  25.5  25.6  28585700     25.6
#>  4 FB     2013-04-30  27.1  27.8  27.0  27.8  36245700     27.8
#>  5 FB     2013-05-31  24.6  25.0  24.3  24.4  35925000     24.4
#>  6 FB     2013-06-28  24.7  25.0  24.4  24.9  96778900     24.9
#>  7 FB     2013-07-31  38.0  38.3  36.3  36.8 154828700     36.8
#>  8 FB     2013-08-30  42.0  42.3  41.1  41.3  67735100     41.3
#>  9 FB     2013-09-30  50.1  51.6  49.8  50.2 100095000     50.2
#> 10 FB     2013-10-31  47.2  52.0  46.5  50.2 248809000     50.2
#> # ... with 38 more rows

# Maybe you don't want to lose the rest of the month's information,
# and instead you'd like to take the average of every column for each month
FB %>%
  select(-symbol) %>%
  collapse_by("monthly") %>%
  group_by(date) %>%
  summarise_all(mean)
#> # A time tibble: 48 x 7
#> # Index: date
#>    date        open  high   low close   volume adjusted
#>    <date>     <dbl> <dbl> <dbl> <dbl>    <dbl>    <dbl>
#>  1 2013-01-31  30.2  30.8  29.8  30.3 79802462     30.3
#>  2 2013-02-28  28.3  28.6  27.7  28.1 50402095     28.1
#>  3 2013-03-28  26.9  27.2  26.5  26.8 36359025     26.8
#>  4 2013-04-30  26.6  27.0  26.2  26.6 33568600     26.6
#>  5 2013-05-31  26.4  26.6  25.9  26.1 44640673     26.1
#>  6 2013-06-28  24.0  24.3  23.7  23.9 39416575     23.9
#>  7 2013-07-31  27.7  28.2  27.4  27.9 65364414     27.9
#>  8 2013-08-30  38.7  39.3  38.2  38.7 61136095     38.7
#>  9 2013-09-30  45.5  46.3  44.9  45.8 79154190     45.8
#> 10 2013-10-31  50.7  51.5  49.7  50.5 88375435     50.5
#> # ... with 38 more rows

# Perform a 5 period rolling average
mean_5 <- rollify(mean, window = 5)
mutate(FB, roll_mean = mean_5(adjusted))
#> # A time tibble: 1,008 x 9
#> # Index: date
#>    symbol date        open  high   low close    volume adjusted roll_mean
#>    <chr>  <date>     <dbl> <dbl> <dbl> <dbl>     <dbl>    <dbl>     <dbl>
#>  1 FB     2013-01-02  27.4  28.2  27.4  28.0  69846400     28.0      NA  
#>  2 FB     2013-01-03  27.9  28.5  27.6  27.8  63140600     27.8      NA  
#>  3 FB     2013-01-04  28.0  28.9  27.8  28.8  72715400     28.8      NA  
#>  4 FB     2013-01-07  28.7  29.8  28.6  29.4  83781800     29.4      NA  
#>  5 FB     2013-01-08  29.5  29.6  28.9  29.1  45871300     29.1      28.6
#>  6 FB     2013-01-09  29.7  30.6  29.5  30.6 104787700     30.6      29.1
#>  7 FB     2013-01-10  30.6  31.5  30.3  31.3  95316400     31.3      29.8
#>  8 FB     2013-01-11  31.3  32.0  31.1  31.7  89598000     31.7      30.4
#>  9 FB     2013-01-14  32.1  32.2  30.6  31.0  98892800     31.0      30.7
#> 10 FB     2013-01-15  30.6  31.7  29.9  30.1 173242600     30.1      30.9
#> # ... with 998 more rows

# Create a time series
# Every other day in 2013
create_series(~'2013', '2 day')
#> # A time tibble: 183 x 1
#> # Index: date
#>    date               
#>    <dttm>             
#>  1 2013-01-01 00:00:00
#>  2 2013-01-03 00:00:00
#>  3 2013-01-05 00:00:00
#>  4 2013-01-07 00:00:00
#>  5 2013-01-09 00:00:00
#>  6 2013-01-11 00:00:00
#>  7 2013-01-13 00:00:00
#>  8 2013-01-15 00:00:00
#>  9 2013-01-17 00:00:00
#> 10 2013-01-19 00:00:00
#> # ... with 173 more rows
```

Grouping
--------

Groups created through `dplyr::group_by()` are supported throughout the package. Because `collapse_index()` is just adding a column you can group on, all `dplyr` functions are supported.

``` r
# Facebook, Amazon, Netflix and Google stocks
data(FANG)

# Summarise by period and by group
FANG %>% 
  as_tbl_time(date) %>%
  group_by(symbol) %>%
  
  # Collapse to yearly
  collapse_by("year") %>%
  
  # Additionally group by date (yearly)
  group_by(date, add = TRUE) %>%
  
  # Perform a yearly summary for each symbol
  summarise(
    adj_min   = min(adjusted),
    adj_max   = max(adjusted),
    adj_range = adj_max - adj_min
  )
#> # A time tibble: 16 x 5
#> # Index:  date
#> # Groups: symbol [?]
#>    symbol date       adj_min adj_max adj_range
#>    <chr>  <date>       <dbl>   <dbl>     <dbl>
#>  1 AMZN   2013-12-31   248     404       156  
#>  2 AMZN   2014-12-31   287     407       120  
#>  3 AMZN   2015-12-31   287     694       407  
#>  4 AMZN   2016-12-30   482     844       362  
#>  5 FB     2013-12-31    22.9    58.0      35.1
#>  6 FB     2014-12-31    53.5    81.4      27.9
#>  7 FB     2015-12-31    74.1   109        35.0
#>  8 FB     2016-12-30    94.2   133        39.1
#>  9 GOOG   2013-12-31   351     560       209  
#> 10 GOOG   2014-12-31   495     609       114  
#> 11 GOOG   2015-12-31   493     777       284  
#> 12 GOOG   2016-12-30   668     813       145  
#> 13 NFLX   2013-12-31    13.1    54.4      41.2
#> 14 NFLX   2014-12-31    44.9    69.2      24.3
#> 15 NFLX   2015-12-31    45.5   131        85.4
#> 16 NFLX   2016-12-30    82.8   128        45.6
```

Index order
-----------

`tibbletime` assumes that your dates are in *ascending order*. A warning will be generated if they are not when you use a function where order is relevant. We do this for speed purposes and to not force a change on the user's dataset by sorting for them.

Vignettes
---------

1.  [Time-based filtering](https://business-science.github.io/tibbletime/articles/TT-01-time-based-filtering.html)

2.  [Changing periodicity](https://business-science.github.io/tibbletime/articles/TT-02-changing-time-periods.html)

3.  [Rolling calculations](https://business-science.github.io/tibbletime/articles/TT-03-rollify-for-rolling-analysis.html)

4.  [Use with dplyr](https://business-science.github.io/tibbletime/articles/TT-04-use-with-dplyr.html)
