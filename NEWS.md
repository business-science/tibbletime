## tibbletime 0.0.2.9000

* Bug Fixes

    * Ensure that `tidyr::spread()` passes the `fill` argument through.

## tibbletime 0.0.2

* New functionality
  
    * `time_floor()` and `time_ceiling()` are convenient wrappers to 
    `lubridate` functions for altering dates to period boundaries.
  
    * `time_unnest()` is used to specifically unnest a `tibble` object
    with a list-column of `tbl_time` objects.
  
    * `create_series()` allows the user to create a `tbl_time` object with
    a regularly spaced sequence of dates.
    
    * `time_group()` has become the workhorse function for creating time based
    groups used in changing periodicity and other grouped 
    time based calculations.
    
    * `time_summarise()` and `tmap()` now also accept a formula-based `period`.
    
    * `as_period()` now accepts a formula-based `period` that provides an 
    incredible amount of flexibility in creating groups. (#9, #14, #15)

    * `rollify()` creates a rolling version of any function for 
    use in `dplyr::mutate()`. (#7)

* General

    * You now have to explicitely load `dplyr` or `tidyr` to use any functions 
    from those packages. Previously they were reexported, but this seems 
    unnecessary.

    * Added vignettes on intro, filtering, and `as_period()`.
    
    * Added more extensive `dplyr` support.
    
    * Speed increases for `as_period()` and `create_series()`.

    * Internal global utilities moved to `utils.R`.

    * Added test coverage. (#2)

    * Added package documentation page. (#3)

    * Added versions to all imported packages.

* Bug Fixes

    * Fixed an issue with `[` in combination with `tibble::add_column()`. Use 
    `tibble (>= 1.3.4.9001)` for correct behavior.

    * Fixed a bug where using `tidyr::nest()` would cause the nested tibbles
    to lose their time attributes.

    * Fix a bug where time_filter(data, ~yyyy-mm-dd) would be parsed as
    `yyyy-mm-dd 00:00:00 ~ yyyy-mm-dd 00:00:00` instead of 
    `yyyy-mm-dd 00:00:00 ~ yyyy-mm-dd 23:59:59`.

    * Fix a bug with as.Date / as.POSIXct operator collision in `time_filter()`.

## tibbletime 0.0.1 

* Initial release of `tibbletime`, a package for time aware tibbles.
