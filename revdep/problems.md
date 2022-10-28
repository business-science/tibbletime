# anomalize

<details>

* Version: 0.2.2
* GitHub: https://github.com/business-science/anomalize
* Source code: https://github.com/cran/anomalize
* Date/Publication: 2020-10-20 18:50:03 UTC
* Number of recursive dependencies: 199

Run `cloud_details(, "anomalize")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘anomalize-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: plot_anomalies
    > ### Title: Visualize the anomalies in one or multiple time series
    > ### Aliases: plot_anomalies
    > 
    > ### ** Examples
    > 
    > 
    ...
    > #### MULTIPLE TIME SERIES ####
    > tidyverse_cran_downloads %>%
    +     time_decompose(count, method = "stl") %>%
    +     anomalize(remainder, method = "iqr") %>%
    +     time_recompose() %>%
    +     plot_anomalies(time_recomposed = TRUE, ncol = 3)
    Error in if (message) message(glue::glue("Converting from {cl} to {class(data)[[1]]}.\n                                    Auto-index message: index = {idx}")) : 
      argument is not interpretable as logical
    Calls: %>% ... time_recompose.grouped_df -> prep_tbl_time -> prep_tbl_time.data.frame
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
       5.   ├─anomalize::prep_tbl_time(data, message = message)
       6.   └─anomalize:::prep_tbl_time.data.frame(data, message = message)
      ── Error ('test-time_recompose.R:9'): time_recompose works on grouped_tbl_time ──
      Error in `if (message) message(glue::glue("Converting from {cl} to {class(data)[[1]]}.\n                                    Auto-index message: index = {idx}"))`: argument is not interpretable as logical
      Backtrace:
          ▆
       1. ├─... %>% time_recompose() at test-time_recompose.R:9:4
       2. ├─anomalize::time_recompose(.)
       3. └─anomalize:::time_recompose.grouped_df(.)
       4.   ├─anomalize::prep_tbl_time(data, message = message)
       5.   └─anomalize:::prep_tbl_time.data.frame(data, message = message)
      
      [ FAIL 2 | WARN 0 | SKIP 1 | PASS 65 ]
      Error: Test failures
      Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘anomalize_methods.Rmd’ using rmarkdown
    --- finished re-building ‘anomalize_methods.Rmd’
    
    --- re-building ‘anomalize_quick_start_guide.Rmd’ using rmarkdown
    Quitting from lines 68-74 (anomalize_quick_start_guide.Rmd) 
    Error: processing vignette 'anomalize_quick_start_guide.Rmd' failed with diagnostics:
    argument is not interpretable as logical
    --- failed re-building ‘anomalize_quick_start_guide.Rmd’
    
    ...
        palette_dark, palette_green, palette_light, scale_color_tq,
        theme_tq, theme_tq_dark, theme_tq_green
    
    --- finished re-building ‘forecasting_with_cleaned_anomalies.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘anomalize_quick_start_guide.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

