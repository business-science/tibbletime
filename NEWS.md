## tibbletime 0.1.0

This is a major update. It introduces a huge number of breaking changes as
we heavily reworked the internals of the package. This should ensure the
longevity of the package and provide maximum flexibility for its use with `dplyr`.
As this was still early in package development with minimal usage, 
and because we had issued a Warning in 
the README of the last update that we may change things, we have not made any
attempt to support backwards compatability. From this point forward, however,
we will support backwards compatability as we feel that we have reached a 
more stable implementation.

With that out of the way, here is a complete list of changes.

* General
    
    * The `period` argument no longer supports the 'period formula'
    (e.g. `1~year`). It added unnecessary complication with little benefit.
    Rather, a character should be used like `'1 year'`. See the documentation
    of `partition_index()` for full details.
    
    * `time_formula` arguments still support the `from ~ to` style syntax,
    but the left and right hand sides must now be characters, rather than
    bare date specifications. In English, rather than `2013 ~ 2014`,
    you must use `'2013' ~ '2014'`. This is easier to program with and 
    also allows you to pass in variables to the time formula, which
    previously did not work well.
    
    * `time_filter()` has become `filter_time()`. This naming is easier to 
    remember now that a suite of `time_*()` functions is not being developed
    and is easier to find with autocompletion.
    
    * `time_group()` and `time_collapse()` have become `partition_index()`
    and `collapse_index()`. Both functions accept `index` vectors and
    are commonly used inside `dplyr::mutate()`.
    
    * `partition_index()` splits an index by period and returns an
    integer vector corresponding to the groups.
    
    * `collapse_index()` collapses an index by period so that all
    observations falling in that interval share the same date. This
    is most useful when used to then group on the index column.
    
    * There is full support for `Date` and `POSIXct` classes as the 
    index, and there is experimental support for
    `yearmon`, `yearqtr`, and `hms` classes.
    
    * `ceiling_index()` and `floor_index()` are thin wrappers 
    around `lubridate` functions of similar names, but they also
    work for `yearmon`, `yearqtr` and `hms`.
    
    * `create_series()` now has an explicit `class` argument.
    
    * `as_period()` gains an `include_endpoints` argument
    for including the last data point if `side = "start"` is
    specified or the first data point if `side = "end"` is used.
    
    * There are a number of new "getter" functions for accessing the
    index and time zone of `tbl_time` objects. These are useful for 
    package development.
    
    * `filter_time()`, `as_period()` and other "getter" functions 
    now use `.tbl_time` as a consistent first argument rather than
    `x`. `collapse_index()` and `partition_index()` use `index` as
    their first arguments.
    
    * Exported `parse_period()` for general use in other related packages.
    
    * Warnings are now generated if the user is not using a sorted index.

* Bug Fixes

    * All `dplyr` functions should now retain the `tbl_time` class and 
    relevant attributes.

    * Ensure that `tidyr::spread()` passes the `fill` argument through.
    
    * Default time zone is now `UTC` rather than `Sys.timezone()` to handle
    a daylight savings issue.

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

    * Fix a bug where filter_time(data, ~yyyy-mm-dd) would be parsed as
    `yyyy-mm-dd 00:00:00 ~ yyyy-mm-dd 00:00:00` instead of 
    `yyyy-mm-dd 00:00:00 ~ yyyy-mm-dd 23:59:59`.

    * Fix a bug with as.Date / as.POSIXct operator collision in `filter_time()`.

## tibbletime 0.0.1 

* Initial release of `tibbletime`, a package for time aware tibbles.
