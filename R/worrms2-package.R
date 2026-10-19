#' Validate and attach World Register of Marine Species taxonomy
#'
#' Match names with [wm_match()], review flagged records, then join accepted
#' taxonomy to observations with [add_wm_taxonomy()]. Live lookups require an
#' internet connection. The package complements the lower-level 'worrms' client.
#'
#' @keywords internal
#' @importFrom dplyr %>% .data mutate case_when distinct rename select
#' @importFrom httr GET content
#' @importFrom jsonlite fromJSON
#' @importFrom mgsub mgsub
#' @importFrom stats na.omit
#' @importFrom stringi stri_enc_isutf8 stri_encode
#' @importFrom stringr str_trim str_remove_all
#' @importFrom worrms wm_records_names
"_PACKAGE"

# Declare data-mask columns while retaining the original readable pipelines.
utils::globalVariables(c(
  "flag", "input_taxa", "eval_taxa", "match_taxa", "worms_match",
  "match_aphiaID", "match_aphia_id", "match_rank", "worms_status",
  "match_worms_status", "valid_aphiaID", "valid_aphia_id", "valid_taxa",
  "valid_rank", "valid_phylum", "valid_class", "valid_order", "valid_family",
  "valid_genus", "valid_species"
))
