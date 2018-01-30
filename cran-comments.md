## Resubmission
This is a resubmission. 

In this version I have removed 'tidyselect' from Imports
as it was not necessary and caused a NOTE. 

There is an error with the 'tibble' package v1.4.1 on r-oldrel on OSX. I 
require >=1.4.1 in 'tibbletime', and all other  builds pass so I would request 
that this is ignored for now. I have opened an issue on the 'tibble' GitHub 
page letting them know about the ERROR.

## Release Summary
This is the fourth release of tibbletime. This is a minor release that
introduces a new function, collapse_by(), but otherwise is maintanence and 
fixing minor bugs.

## Test environments
* local Windows install, R 3.4.1
* local Mac install, R 3.4.3
* ubuntu 12.04 (on travis-ci), R 3.4.3
* win-builder (devel and release)

## R CMD check results

There were no ERRORs, WARNINGs or NOTEs.

    R CMD check results
    0 errors | 0 warnings | 0 note 

    R CMD check succeeded

## Downstream dependencies
None
