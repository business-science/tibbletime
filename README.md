
<!-- README.md is generated from README.Rmd. Please edit that file -->
tibbletime
==========

[![Travis-CI Build Status](https://travis-ci.org/business-science/tibbletime.svg?branch=master)](https://travis-ci.org/business-science/tibbletime.svg?branch=master) [![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/tibbletime)](https://cran.r-project.org/package=tibbletime) [![codecov](https://codecov.io/gh/business-science/tibbletime/branch/master/graph/badge.svg)](https://codecov.io/gh/business-science/tibbletime)

A time aware tibble
-------------------

Built on top of the `tidyverse`, `tibbletime` is an extension that allows for the creation of *time aware tibbles* through the setting of a time index.

Some immediate advantages of this include:

1.  The ability to perform compact time-based subsetting on tibbles.

2.  Quickly summarising and aggregating results by time period (yearly, monthly, every 2 weeks, etc).

3.  Changing the periodicity of a time-based tibble. This means changing from a daily dataset to a monthly or yearly dataset.

4.  Calling functions similar in spirit to the `purrr::map()` family on time-based tibbles.

5.  All functions were designed to support the pipe and to work with packages like `dplyr` and `tidyr`. Each function has also been designed to work with `dplyr::group_by()` allowing for powerful data manipulation.

6.  Modifying functions for rolling analysis.

7.  Quickly creating `tbl_time` time series objects.

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

Getting started
---------------

The first thing to do is to turn your `tibble` into a `tbl_time` object. Notice the specification of the `index` as the `date` column of `FB`.

``` r
library(tibbletime)
library(dplyr)

# Facebook stock prices. Comes with the package
data(FB)

# Convert FB to tbl_time
FB <- FB %>% as_tbl_time(index = date)

FB
#> # A time tibble: 1,008 x 8
#> # Index: date
#>    symbol       date  open  high   low close    volume adjusted
#>     <chr>     <date> <dbl> <dbl> <dbl> <dbl>     <dbl>    <dbl>
#>  1     FB 2013-01-02 27.44 28.18 27.42 28.00  69846400    28.00
#>  2     FB 2013-01-03 27.88 28.47 27.59 27.77  63140600    27.77
#>  3     FB 2013-01-04 28.01 28.93 27.83 28.76  72715400    28.76
#>  4     FB 2013-01-07 28.69 29.79 28.65 29.42  83781800    29.42
#>  5     FB 2013-01-08 29.51 29.60 28.86 29.06  45871300    29.06
#>  6     FB 2013-01-09 29.67 30.60 29.49 30.59 104787700    30.59
#>  7     FB 2013-01-10 30.60 31.45 30.28 31.30  95316400    31.30
#>  8     FB 2013-01-11 31.28 31.96 31.10 31.72  89598000    31.72
#>  9     FB 2013-01-14 32.08 32.21 30.62 30.95  98892800    30.95
#> 10     FB 2013-01-15 30.64 31.71 29.88 30.10 173242600    30.10
#> # ... with 998 more rows
```

There are a number of functions that were designed specifically for `tbl_time` objects. Some of them are:

1.  `time_filter()` - Succinctly filter a tbl\_time object by date.

2.  `time_summarise()` - Similar to dplyr::summarise but with the added benefit of being able to summarise by a time period such as "yearly" or "monthly".

3.  `tmap()` - The family of tmap functions transform a tbl\_time input by applying a function to each column at a specified time interval.

4.  `as_period()` - Convert a tbl\_time object from daily to monthly, from minute data to hourly, and more. This allows the user to easily aggregate data to a less granular level.

5.  `time_collapse()` - When time\_collapse is used, the index of a tbl\_time object is altered so that all dates that fall in a period share a common date.

6.  `rollify()` - Modify a function so that it calculates a value (or a set of values) at specific time intervals. This can be used for rolling averages and other rolling calculations inside the `tidyverse` framework.

7.  `create_series()` - Use shorthand notation to quickly initialize a `tbl_time` object containing a `date` column with a regularly spaced time series.

To look at just a few:

``` r
# Filter for dates from March 2013 to December 2015
FB %>% 
  time_filter(2013-03 ~ 2015)
#> # A time tibble: 716 x 8
#> # Index: date
#>    symbol       date  open  high   low close   volume adjusted
#>  *  <chr>     <date> <dbl> <dbl> <dbl> <dbl>    <dbl>    <dbl>
#>  1     FB 2013-03-01 27.05 28.12 26.81 27.78 54064800    27.78
#>  2     FB 2013-03-04 27.76 28.06 27.44 27.72 32400700    27.72
#>  3     FB 2013-03-05 27.88 28.18 27.21 27.52 40622200    27.52
#>  4     FB 2013-03-06 28.10 28.13 27.35 27.45 33532600    27.45
#>  5     FB 2013-03-07 27.57 28.68 27.47 28.58 74540200    28.58
#>  6     FB 2013-03-08 28.43 28.47 27.73 27.96 44198900    27.96
#>  7     FB 2013-03-11 28.01 28.64 27.83 28.14 35642100    28.14
#>  8     FB 2013-03-12 28.10 28.32 27.60 27.83 27569600    27.83
#>  9     FB 2013-03-13 27.62 27.65 26.92 27.08 39619500    27.08
#> 10     FB 2013-03-14 27.10 27.43 26.83 27.04 27646400    27.04
#> # ... with 706 more rows

# Change from daily to monthly periodicity
FB %>% 
  as_period("monthly")
#> # A time tibble: 48 x 8
#> # Index: date
#>    symbol       date  open  high   low close    volume adjusted
#>  *  <chr>     <date> <dbl> <dbl> <dbl> <dbl>     <dbl>    <dbl>
#>  1     FB 2013-01-02 27.44 28.18 27.42 28.00  69846400    28.00
#>  2     FB 2013-02-01 31.01 31.02 29.63 29.73  85856700    29.73
#>  3     FB 2013-03-01 27.05 28.12 26.81 27.78  54064800    27.78
#>  4     FB 2013-04-01 25.63 25.89 25.28 25.53  22249300    25.53
#>  5     FB 2013-05-01 27.85 27.92 27.31 27.43  64567600    27.43
#>  6     FB 2013-06-03 24.27 24.32 23.71 23.85  35733800    23.85
#>  7     FB 2013-07-01 24.97 25.06 24.62 24.81  20582200    24.81
#>  8     FB 2013-08-01 37.30 38.29 36.92 37.49 106066500    37.49
#>  9     FB 2013-09-03 41.84 42.16 41.51 41.87  48774900    41.87
#> 10     FB 2013-10-01 49.97 51.03 49.45 50.42  98114000    50.42
#> # ... with 38 more rows

# Get the average mean and standard deviation for each year
FB %>%
  time_summarise(period = 1~y,
        adj_mean = mean(adjusted),
        adj_sd   = sd(adjusted))
#> # A time tibble: 4 x 3
#> # Index: date
#>         date  adj_mean    adj_sd
#> *     <date>     <dbl>     <dbl>
#> 1 2013-12-31  35.48115 10.621172
#> 2 2014-12-31  68.76234  7.502259
#> 3 2015-12-31  88.77286 10.211442
#> 4 2016-12-30 117.03587  8.899858

# Perform a 5 period rolling average
mean_5 <- rollify(mean, window = 5)
FB %>%
  mutate(roll_mean = mean_5(adjusted))
#> # A time tibble: 1,008 x 9
#> # Index: date
#>    symbol       date  open  high   low close    volume adjusted roll_mean
#>  *  <chr>     <date> <dbl> <dbl> <dbl> <dbl>     <dbl>    <dbl>     <dbl>
#>  1     FB 2013-01-02 27.44 28.18 27.42 28.00  69846400    28.00        NA
#>  2     FB 2013-01-03 27.88 28.47 27.59 27.77  63140600    27.77        NA
#>  3     FB 2013-01-04 28.01 28.93 27.83 28.76  72715400    28.76        NA
#>  4     FB 2013-01-07 28.69 29.79 28.65 29.42  83781800    29.42        NA
#>  5     FB 2013-01-08 29.51 29.60 28.86 29.06  45871300    29.06    28.602
#>  6     FB 2013-01-09 29.67 30.60 29.49 30.59 104787700    30.59    29.120
#>  7     FB 2013-01-10 30.60 31.45 30.28 31.30  95316400    31.30    29.826
#>  8     FB 2013-01-11 31.28 31.96 31.10 31.72  89598000    31.72    30.418
#>  9     FB 2013-01-14 32.08 32.21 30.62 30.95  98892800    30.95    30.724
#> 10     FB 2013-01-15 30.64 31.71 29.88 30.10 173242600    30.10    30.932
#> # ... with 998 more rows

# Create a time series
# Every other day in 2013
create_series(~2013, 2~d)
#> # A time tibble: 183 x 1
#> # Index: date
#>          date
#>        <date>
#>  1 2013-01-01
#>  2 2013-01-03
#>  3 2013-01-05
#>  4 2013-01-07
#>  5 2013-01-09
#>  6 2013-01-11
#>  7 2013-01-13
#>  8 2013-01-15
#>  9 2013-01-17
#> 10 2013-01-19
#> # ... with 173 more rows
```

Grouping
--------

Groups created through `dplyr::group_by()` are supported throughout the package.

``` r
# Facebook, Amazon, Netflix and Google stocks
data(FANG)

# Summarise by period and by group
FANG %>% 
  as_tbl_time(date) %>%
  group_by(symbol) %>%
  time_summarise(period = "yearly",
        adj_min   = min(adjusted),
        adj_max   = max(adjusted),
        adj_range = adj_max - adj_min)
#> # A time tibble: 16 x 5
#> # Index:  date
#> # Groups: symbol [4]
#>    symbol       date   adj_min   adj_max adj_range
#>  *  <chr>     <date>     <dbl>     <dbl>     <dbl>
#>  1   AMZN 2013-12-31 248.23000 404.39002 156.16002
#>  2   AMZN 2014-12-31 287.06000 407.04999 119.98999
#>  3   AMZN 2015-12-31 286.95001 693.96997 407.01996
#>  4   AMZN 2016-12-30 482.07001 844.35999 362.28998
#>  5     FB 2013-12-31  22.90000  57.96000  35.06000
#>  6     FB 2014-12-31  53.53000  81.45000  27.92000
#>  7     FB 2015-12-31  74.05000 109.01000  34.96000
#>  8     FB 2016-12-30  94.16000 133.28000  39.12000
#>  9   GOOG 2013-12-31 351.08451 559.79618 208.71167
#> 10   GOOG 2014-12-31 495.39224 609.47654 114.08430
#> 11   GOOG 2015-12-31 492.55224 776.59998 284.04774
#> 12   GOOG 2016-12-30 668.26001 813.10999 144.84998
#> 13   NFLX 2013-12-31  13.14429  54.36857  41.22429
#> 14   NFLX 2014-12-31  44.88714  69.19857  24.31143
#> 15   NFLX 2015-12-31  45.54714 130.92999  85.38285
#> 16   NFLX 2016-12-30  82.79000 128.35001  45.56001
```

Index order
-----------

`tibbletime` assumes that your dates are in *ascending order*. A warning will be generated if they are not when you try and use any `time_*()` function. We do this for speed purposes and to not force a change on the user's dataset by sorting for them.

Vignettes
---------

There are currently 4 vignettes for `tibbletime`.

1.  [Introduction to tibbletime](https://business-science.github.io/tibbletime/articles/TT-00-intro-to-tibbletime.html)

2.  [Time-based filtering](https://business-science.github.io/tibbletime/articles/TT-01-time-based-filtering.html)

3.  [Changing periodicity](https://business-science.github.io/tibbletime/articles/TT-02-changing-time-periods.html)

4.  [Rolling calculations](https://business-science.github.io/tibbletime/articles/TT-03-rollify-for-rolling-analysis.html)

Warning
-------

This package is still going through active development and is subject to change. Use at your own risk. Reproducible bug reports and suggestions for new features are welcome!
