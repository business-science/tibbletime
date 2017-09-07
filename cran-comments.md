## Resubmission
This is a resubmission. In this version I have:

* Written package names in the DESCRIPTION Description in single quotes. Note that `tibbles` is not a package name in itself (while 'tibble' is) so I did not single quote that.

* Attempted to remove all double spaces in the DESCRIPTION Description. I had followed the 4 spaces guideline but it seems to be adding an extra space. I now align all new lines under the 's' in 'Description' like how it is done in R Extensions.

* Looked at the reexports.Rd file. I cannot figure out what is wrong with the aliases. I regenerated it using `devtools::document()` but got the exact same file. I compared with `tidygraph` which does a similar thing but they look the same. I built the package and looked at the documentation page ?reexports and all of the links work correctly. Can you help me?

## Release Summary
This is the initial release of tibbletime.

## Test environments
* local Windows install, R 3.4.1
* local Mac install, R 3.4.1
* ubuntu 12.04 (on travis-ci), R 3.4.1
* win-builder (devel and release)

## R CMD check results
On win-builder, a NOTE about "New submission" was shown because Davis Vaughan
is the main author.

Otherwise there were no ERRORs, WARNINGs or NOTEs.

    R CMD check results
    0 errors | 0 warnings | 0 note 

    R CMD check succeeded

## Downstream dependencies
None
