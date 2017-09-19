## tibbletime 0.0.1.9000

* New functionality

    * `rollify` creates a rolling version of any function for 
    use in `dplyr::mutate`.

* General

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
