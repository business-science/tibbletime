## Resubmission

I accidentally shipped the entire revdep/ folder alongside the package. I have
added it to '.Rbuildignore' now.

## Release Summary

This is a minor release of 'tibbletime'. It fixes a few small bugs regarding
the new version of 'tibble', and changes a test in preparation for 
'dplyr' 0.8.0.

I have also changed the maintainer email to reflect a job change.

## Test environments
* local Mac install, R 3.5.1
* ubuntu 14.04.5 (on travis-ci) (3.4.4, devel, release)
* win-builder (devel and release)

## R CMD check results

There were no ERRORs, WARNINGs or NOTEs.

    R CMD check results
    0 errors | 0 warnings | 0 note 

    R CMD check succeeded

## Revdep checks

The 3 reverse dependencies passed with no errors, and the warnings / notes
were unrelated to this update.

|package                            |version |error |warning |note |
|:----------------------------------|:-------|:-----|:-------|:----|
|[anomalize](problems.md#anomalize) |0.1.1   |      |        |1    |
|[tidyquant](problems.md#tidyquant) |0.5.5   |      |1       |     |
|tsbox                              |0.0.3   |      |        |     |


