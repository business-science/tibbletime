## Release Summary

This is a minor release of 'tibbletime'. It mainly updates the `tidyr::nest()` 
and `tidyr::unnest()` methods for compatability with tidyr 1.0.0.

## Test environments
* local Mac install, R 3.6.0
* ubuntu 14.04.5 (on travis-ci) (3.6.0, devel, release)
* win-builder (devel and release)

## R CMD check results

There were no ERRORs, WARNINGs or NOTEs.

    R CMD check results
    0 errors | 0 warnings | 0 note 

    R CMD check succeeded

## Revdep checks

The 4 revdeps have been checked, and nothing has been broken by this release.
This should free up anomalize to be fixed as well (tidyr 1.0.0 broke it).

