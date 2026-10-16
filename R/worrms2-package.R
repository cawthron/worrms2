#' Validate and attach World Register of Marine Species taxonomy
#'
#' Match names with [wm_match()], review flagged records, then join accepted
#' taxonomy to observations with [add_wm_taxonomy()]. Live lookups require an
#' internet connection. The package complements the lower-level 'worrms' client.
#'
#' @keywords internal
#' @importFrom dplyr %>% .data
"_PACKAGE"
