# This is the starting message when loading

.onAttach <- function(libname, pkgname) {
  packageStartupMessage("Welcome to worrms2.
                        
This is an R package for checking marine taxonomic names against the WoRMS database.
                        
It heavily builds on the official 'worrms' package (https://cran.r-project.org/package=worrms).

-------------------------------------------------------------------------------------------------------------------------                                                
It contains 3 main functions:
-> wm_match():         Launches full validation of taxonomic names against WoRMS
-> add_wm_taxonomy():  Matches validation result against another data frame
-> worrms2_method():   Returns a method blurb that can be used for reports and papers

-------------------------------------------------------------------------------------------------------------------------                        
For more information run ?wm_match(), ?add_wm_taxonomy(), ?worrms2_method()
                        
or contact Fabio Weiss (fabio.weiss@cawthron.org.nz)." )}
