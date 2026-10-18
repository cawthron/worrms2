#' Return template text describing taxonomic validation
#'
#' Provide methodology text and a reference for use in reports and papers.
#'
#' @return A single character string containing the methodology template and
#'   reference, separated by line breaks.
#' @details Replace `ENTER DATE` and `Table X (Appendix)` before using the text.
#'   Check that the description and cited package version match the workflow
#'   actually used. This function does not run or verify taxonomic validation.
#' @seealso [wm_match()], [add_wm_taxonomy()]
#' @examples
#' methodology <- worrms2_method()
#' cat(methodology)
#' @export
worrms2_method <- function() {
message(  
  "
_________________________________________________________________________  
CAUTION: Replace ENTER DATE and Table X (Appendix) before using the text.
_________________________________________________________________________

Prior to data analysis, all taxonomic names were systematically validated and, where necessary, updated using the World Register of Marine Species (WoRMS; https://www.marinespecies.org) as the authoritative reference. This process was conducted on ENTER DATE using an automated workflow implemented via the ‘worrms’ R package (Chamberlain & Vanhoorne, 2023) and the WoRMS RESTful web services (https://www.marinespecies.org/rest/). Comprehensive documentation of the validation procedure, including query parameters and resolution outcomes, is provided in Table X (Appendix).

References
Chamberlain, S., Vanhoorne, B. (2023) worrms: World Register of Marine Species (WoRMS) Client. R package version 0.4.3. https://docs.ropensci.org/worrms/)"
)  }


  
