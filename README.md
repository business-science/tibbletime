
<!-- README.md is generated from README.Rmd. Please edit that file -->
tibbletime
==========

[![Travis-CI Build Status](https://travis-ci.org/business-science/tibbletime.svg?branch=master)](https://travis-ci.org/business-science/tibbletime.svg?branch=master) [![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/tibbletime)](https://cran.r-project.org/package=tibbletime) [![codecov](https://codecov.io/gh/business-science/tibbletime/branch/master/graph/badge.svg)](https://codecov.io/gh/business-science/tibbletime)

A time aware tibble
-------------------

Built on top of the `tidyverse`, `tibbletime` is an extension that allows for the creation of *time aware tibbles* through the setting of a time index.

Some immediate advantages of this include:

1.  The ability to perform compact time based subsetting on tibbles.

2.  Quickly summarising and aggregating results by time period (yearly, monthly, etc).

3.  Changing the periodicity of a time based tibble. This means changing from a daily dataset to a monthly or yearly dataset.

4.  Calling functions similar in spirit to the `map` family from `purrr` on time based tibbles.

5.  All functions were designed to support the pipe and to work with packages like `dplyr` and `tidyr`.

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

# Facebook stock prices. Comes with the package
data(FB)

# Convert FB to tbl_time
FB <- FB %>% as_tbl_time(index = date)

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

1.  `time_filter` - Succinctly filter a tbl\_time object by date.

2.  `time_summarise` - Similar to dplyr::summarise but with the added benefit of being able to summarise by a time period such as "yearly" or "monthly".

3.  `tmap` - The family of tmap functions transform a tbl\_time input by applying a function to each column at a specified time interval.

4.  `as_period` - Convert a tbl\_time object from daily to monthly, from minute data to hourly, and more. This allows the user to easily aggregate data to a less granular level.

5.  `time_collapse` - When time\_collapse is used, the index of a tbl\_time object is altered so that all dates that fall in a period share a common date.

To look at just a few:

``` r
# Filter for dates from March 2013 to December 2015
FB %>% 
  time_filter(2013-03 ~ 2015)
#> # A time tibble: 716 x 8
#> # Index: date
#>    symbol date        open  high   low close   volume adjusted
#>  * <chr>  <date>     <dbl> <dbl> <dbl> <dbl>    <dbl>    <dbl>
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
FB %>% 
  as_period("monthly")
#> # A time tibble: 48 x 8
#> # Index: date
#>    symbol date        open  high   low close    volume adjusted
#>  * <chr>  <date>     <dbl> <dbl> <dbl> <dbl>     <dbl>    <dbl>
#>  1 FB     2013-01-02  27.4  28.2  27.4  28.0  69846400     28.0
#>  2 FB     2013-02-01  31.0  31.0  29.6  29.7  85856700     29.7
#>  3 FB     2013-03-01  27.0  28.1  26.8  27.8  54064800     27.8
#>  4 FB     2013-04-01  25.6  25.9  25.3  25.5  22249300     25.5
#>  5 FB     2013-05-01  27.8  27.9  27.3  27.4  64567600     27.4
#>  6 FB     2013-06-03  24.3  24.3  23.7  23.8  35733800     23.8
#>  7 FB     2013-07-01  25.0  25.1  24.6  24.8  20582200     24.8
#>  8 FB     2013-08-01  37.3  38.3  36.9  37.5 106066500     37.5
#>  9 FB     2013-09-03  41.8  42.2  41.5  41.9  48774900     41.9
#> 10 FB     2013-10-01  50.0  51.0  49.5  50.4  98114000     50.4
#> # ... with 38 more rows

# Get the average mean and standard deviation for each year
FB %>%
  time_summarise(period = "yearly",
        adj_mean = mean(adjusted),
        adj_sd   = sd(adjusted))
#> # A time tibble: 4 x 3
#> # Index: date
#>   date       adj_mean adj_sd
#> * <date>        <dbl>  <dbl>
#> 1 2013-12-31     35.5  10.6 
#> 2 2014-12-31     68.8   7.50
#> 3 2015-12-31     88.8  10.2 
#> 4 2016-12-30    117     8.90
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
#> # Index: date
#> # Groups: symbol [4]
#>    symbol date       adj_min adj_max adj_range
#>  * <chr>  <date>       <dbl>   <dbl>     <dbl>
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

Warning
-------

This package is still going through active development and is subject to change. Use at your own risk. Reproducible bug reports are welcome.

Vignettes and more functionality are coming soon. Stay tuned.
