## Release Summary
This is the second release of tibbletime. This release introduces new functionality for changing periodicity, creating series, creating rolling functions and a number of other things. It also fixes a few bugs present in the initial release, and adds vignettes.

## Test environments
* local Windows install, R 3.4.1
* local Mac install, R 3.4.1
* ubuntu 12.04 (on travis-ci), R 3.4.1
* win-builder (devel and release)

## R CMD check results

On win-builder there were 2 errors and 1 warning, all related to a timezone
error, "Evaluation error: Unrecognized timezone: "Europe/Berlin"." I do not
think that this is caused by tibbletime.

Otherwise there were no ERRORs, WARNINGs or NOTEs.

    R CMD check results
    0 errors | 0 warnings | 0 note 

    R CMD check succeeded

## Downstream dependencies
None
