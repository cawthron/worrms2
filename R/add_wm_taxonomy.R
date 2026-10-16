# Join helper retained from the standalone script supplied for packaging.

#' Add validated WoRMS taxonomy to observations
#'
#' @param data Data frame containing original taxonomic names.
#' @param validation_table Validation data frame returned by [wm_match()].
#' @param by_taxa Name of the taxon column in `data`. Defaults to `"taxa"`.
#' @return A data.frame with phylum, class, order, family, genus, species, rank,
#'   aphia_id, and taxa first, followed by the other observation columns.
#'   The original taxon column is renamed to original_taxa. Unmatched or missing
#'   accepted names fall back to the original input label after trimming.
#' @details This preserves the standalone helper's join behaviour. Observation
#'   names are trimmed before joining to `validation_table$input_taxa`. Use
#'   consistently trimmed names when calling both functions: validation keys
#'   are not trimmed by this helper. Repeated observation names are supported.
#'
#'   Validation keys should be unique, as they are in the output of [wm_match()].
#'   Duplicated validation keys can expand the number of output rows. Existing
#'   phylum, class, order, family, and genus columns are replaced. Other column
#'   collisions follow [dplyr::left_join()] suffix rules; start with observations
#'   without species, rank, aphia_id, original_taxa, or valid_taxa columns to avoid
#'   collisions with generated fields.
#' @seealso [wm_match()]
#' @examples
#' observations <- data.frame(taxa = c("Example taxon", "Unmatched"), count = c(3, 1))
#' validation <- data.frame(
#'   input_taxa = "Example taxon", valid_taxa = "Example taxon",
#'   valid_phylum = "Example phylum", valid_class = NA_character_,
#'   valid_order = NA_character_, valid_family = NA_character_,
#'   valid_genus = "Example", valid_species = "Example taxon",
#'   valid_rank = "Species", valid_aphia_id = "1"
#' )
#' add_wm_taxonomy(observations, validation)
#' @export
add_wm_taxonomy <- function(data, validation_table, by_taxa = "taxa") {
  required <- c("valid_phylum", "valid_class", "valid_order", "valid_family",
                 "valid_genus", "valid_species", "valid_rank", "valid_aphia_id",
                 "input_taxa", "valid_taxa")
  if (!is.data.frame(validation_table) || !all(required %in% names(validation_table))) {
    stop("'validation_table' must be the output of worrms2::wm_match().", call. = FALSE)
  }
  if (!is.data.frame(data) || !is.character(by_taxa) || length(by_taxa) != 1L ||
      is.na(by_taxa) || !by_taxa %in% names(data)) {
    stop("'data' must be a data frame containing the column named by 'by_taxa'.",
         call. = FALSE)
  }

  # Name fields explicitly rather than renaming by column position.
  taxonomy <- validation_table %>% dplyr::transmute(
    phylum = .data$valid_phylum, class = .data$valid_class,
    order = .data$valid_order, family = .data$valid_family,
    genus = .data$valid_genus, species = .data$valid_species,
    rank = .data$valid_rank, aphia_id = .data$valid_aphia_id,
    taxa = .data$input_taxa, valid_taxa = .data$valid_taxa
  )

  # Keep the original trimming, unmatched fallback, and left-join semantics.
  data %>%
    dplyr::select(-dplyr::any_of(c("phylum", "class", "order", "genus", "family"))) %>%
    dplyr::rename(taxa = dplyr::all_of(by_taxa)) %>%
    dplyr::mutate(taxa = stringr::str_trim(.data$taxa)) %>%
    dplyr::left_join(taxonomy, by = "taxa") %>%
    dplyr::rename(original_taxa = dplyr::all_of("taxa"),
                   taxa = dplyr::all_of("valid_taxa")) %>%
    dplyr::mutate(taxa = dplyr::case_when(
      is.na(.data$taxa) ~ .data$original_taxa,
      TRUE ~ .data$taxa
    )) %>%
    dplyr::relocate(dplyr::all_of("phylum"):dplyr::all_of("taxa")) %>%
    as.data.frame()
}
