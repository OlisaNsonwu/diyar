## Resubmission
This is a resubmission. In this version I have:
* Addressed these CRAN comment
    + Thanks, please add a few more details about the methods used in this package in your Description text.
    + If you want to use the GPL-3 lisence, please write License: GPL-3 in your DESCRIPTION file and omit the LICENSE file in your  package.
* Corrected typos in some vignette

### Response to CRAN comments
* If there are references describing the (theoretical background of) 
methods in your package, please add these in the Description field of
your DESCRIPTION file in the form
authors (year) <doi:...>
authors (year) <arXiv:...>
authors (year, ISBN:...)
with no space after 'doi:', 'arXiv:' and angle brackets for auto-linking.
    + No reference
    
## Resubmission
This is a resubmission. In this version I have:
* Addressed this CRAN comment - Please omit the redundant "A set of functions to" at the beginning of the Descrition field.

### Response to CRAN comments
* "Is there some reference about the method you can add in the Description
field in the form Authors (year) <doi:.....>?
    + None published. Vignettes explaining the principles behind the implementation of these functions are included. 

## Test environments
* Ubuntu 16.04.6 LTS (on travis-ci), R 3.6.1
* win-builder (devel and release)

## R CMD check results
There were no ERRORs or WARNINGs on the Ubuntu and win-builder test environments. 

There was 1 NOTE:
Could not eliminate this. No messaged dsiplayed. This is a first submission.

## Downstream dependencies
This is not a new version of an existing package