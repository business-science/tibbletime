## tibbletime 0.0.1.9000

* New functionality
  
    * `create_series()` allows the user to create a `tbl_time` object with
    a regularly spaced sequence of dates.
    
    * `time_group()` has become the workhorse function for creating time based
    groups used in changing periodicity and eventually other time based 
    groupwise operations.
    
    * `as_period()` now accepts a formula based `period` that provides an 
    incredible amount of flexibility in creating groups.

    * `rollify` creates a rolling version of any function for 
    use in `dplyr::mutate`.

* General

    * Internal global utilities moved to `utils.R`

    * Added complete test coverage

    * Added package documentation page.

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
