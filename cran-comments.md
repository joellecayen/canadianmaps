# Resubmission 2023-07-07
## R CMD check results

0 errors v | 0 warnings v | 0 notes v

## Test environments
- R-hub windows-x86_64-devel (r-devel)
- R-hub ubuntu-gcc-release (r-release)
- R-hub fedora-clang-devel (r-devel)

## Spell check
devtools::spell_check()
#### COMMENTS
All of the flagged words have been double checked are are spelled correctly.


# Initial release
## R CMD check results

0 errors | 0 warnings | 2 notes

* This is a new release.

## Test environments
- R-hub windows-x86_64-devel (r-devel)
- R-hub ubuntu-gcc-release (r-release)
- R-hub fedora-clang-devel (r-devel)

### Note 1
* On windows-x86_64-devel (r-devel), ubuntu-gcc-release (r-release), fedora-clang-devel (r-devel)
  checking CRAN incoming feasibility ... NOTE
  Maintainer: 'Joelle Cayen <joelle.cayen@phac-aspc.gc.ca>'
  
  New submission
  
  Version contains large components (1.0.0)
  
  Possibly misspelled words in DESCRIPTION:
    sortation (10:82)

#### COMMENTS    
Word 'sortation' is not misspelled (Forward Sortation Area = FSA, the first 3 digits in Canadian postal codes)


### Note 2
 On windows-x86_64-devel (r-devel)
 checking for detritus in the temp directory ... NOTE
 Found the following files/directories:
    'lastMiKTeXException'

#### COMMENTS    
Unsure what this file has to do with the package. Assuming it's not related.


## Spell check
devtools::spell_check()

  WORD        FOUND IN
FSA         FSA.Rd:6,10,26
            geom_fsa.Rd:5,19,22
ggplot      scale_color_map.Rd:19
            scale_fill_map.Rd:19
            theme_map.Rd:15
            theme_wallis.Rd:10
            description:1
sortation   description:1

#### COMMENTS
All of the above flagged words have been double checked are are spelled correctly.


