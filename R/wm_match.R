# Taxonomic validation, refactored from Fabio Weiss's original WoRMS script.
# Package dependencies are declared in DESCRIPTION and NAMESPACE.

#' Validate taxonomic names against WoRMS using tidyverse data pipelines
#' 
#' Very similar to `worrms::wm_records_taxamatch` while being able to handle more names at once, providing some quality control, and returning outputs as data frame (instead of list).
#'
#' @param taxa Character vector of taxonomic names without missing values.
#' @param eval_as Named character vector mapping original names to lookup names.
#'   Use `NULL` for no overrides. The original default is retained.
#' @param prio_marine Logical; prefer marine records when available. Defaults
#'   to `TRUE`.
#' @param prio_accepted Logical; prefer accepted records when available.
#' @param exclude_extinct Logical; discard explicitly extinct records.
#' @param batch_size Integer from 1 to 50; maximum names per API request.
#' @return A data.frame with the original 17 validation columns, one row per
#'   distinct input name, in first-occurrence order. Aphia IDs are character.
#' @details The original cleaning rules, match labels and priority order are
#'   retained: exclude extinct records, prefer marine, then prefer accepted.
#'   Remaining ties use the first API record and are flagged. As in the original,
#'   exact searches use `marine_only = TRUE`, whereas TAXAMATCH searches use
#'   `marine_only = FALSE`. Thus `prio_marine = FALSE` disables preference within
#'   returned candidates, but does not broaden the exact-search endpoint.
#'
#'   Names sharing a cleaned lookup name share requests. Fuzzy searches are
#'   batched, and accepted records are cached within this call. Exact responses
#'   with 50 records fall back to TAXAMATCH, as in the original.
#'
#'   Failed batches fall back to one request per name. Individual failures warn;
#'   unsuccessful matches and missing accepted taxonomy are flagged. Unlike the
#'   original, failed accepted-record requests leave accepted taxonomy missing.
#'   Empty cleaned names are not submitted. Review all flagged results.
#'
#'   Progress messages report completed exact batches, original labels and row
#'   positions submitted for fuzzy matching, elapsed time, and a review reminder.
#'   Batch counts refer to distinct cleaned names; row positions refer to the
#'   distinct original inputs. No requests are made for an empty input.
#' @section Output columns:
#' \describe{
#'   \item{flag}{Integer review indicator: 0 or 1.}
#'   \item{input_taxa, eval_taxa}{Original and cleaned lookup names.}
#'   \item{match_taxa, worms_match, match_worms_status}{Matched name, match
#'     method, and the matched record's taxonomic status.}
#'   \item{match_aphia_id, match_rank}{Matched record's identifier and rank.}
#'   \item{valid_taxa, valid_aphia_id, valid_rank}{Accepted or parent record's
#'     name, identifier, and rank. Identifiers are stored as character.}
#'   \item{valid_phylum, valid_class, valid_order, valid_family, valid_genus}{
#'     Classification fields from the accepted or parent record.}
#'   \item{valid_species}{Accepted name when its rank is Species; otherwise NA.}
#' }
#' @examples
#' wm_match(character())
#' \dontrun{
#' validation <- wm_match(c("Abra alba", "Bivalvia Unid. (juv)"),
#'                        eval_as = c("Bivalvia Unid. (juv)" = "Bivalvia"))
#' validation[validation$flag == 1L, ]
#' }
#' @seealso [add_wm_taxonomy()]
#' @export
wm_match <- function(taxa, eval_as = c(default = "default"),
                     prio_marine = TRUE, prio_accepted = TRUE,
                     exclude_extinct = TRUE, batch_size = 49) {
  .wm_tidy_validate(taxa, eval_as, prio_marine, prio_accepted,
                    exclude_extinct, batch_size)
  started <- Sys.time()

  # Clean once, preserving the original mgsub patterns and override order.
  input_taxa <- .wm_tidy_utf8(taxa)
  input <- tibble::tibble(input_taxa = input_taxa) %>%
    dplyr::mutate(
      eval_taxa = stringr::str_trim(.data$input_taxa),
      eval_taxa = stringr::str_replace_all(.data$eval_taxa, "\\s+", " "),
      eval_taxa = mgsub::mgsub(
        .data$eval_taxa, c(" sp\\.", " cf", " cf\\.", " spp\\.", " spp"), rep("", 5)
      ),
      eval_taxa = dplyr::if_else(
        .data$input_taxa %in% names(eval_as),
        unname(eval_as[match(.data$input_taxa, names(eval_as))]), .data$eval_taxa
      ),
      eval_taxa = stringr::str_replace_all(.data$eval_taxa, "[[:punct:]]", "")
    ) %>%
    dplyr::distinct()

  # A typed zero-row template also defines the output for an empty input.
  template <- .wm_tidy_result_row()[0, ]
  if (!nrow(input)) {
    return(input %>%
             dplyr::bind_cols(template) %>%
             dplyr::relocate(dplyr::all_of("flag")) %>%
             as.data.frame())
  }
  lookup_names <- input %>% dplyr::distinct(.data$eval_taxa) %>% dplyr::pull("eval_taxa")
  query_index <- which(nzchar(stringr::str_trim(lookup_names)))
  exact_records <- vector("list", length(lookup_names))
  message("Direct matching with WoRMS ...")
  exact_records[query_index] <- .wm_tidy_batches(
    lookup_names[query_index], "exact", batch_size,
    after_batch = function(index, batch_number, batch_count) {
      message(sprintf("Batch %d/%d", batch_number, batch_count))
    }
  )
  selections <- lapply(seq_along(lookup_names), function(i) {
    .wm_tidy_select(exact_records[[i]], lookup_names[i], TRUE,
                     prio_marine, prio_accepted, exclude_extinct)
  })

  # Only unresolved names use TAXAMATCH; keep NULL slots aligned with inputs.
  unresolved <- query_index[vapply(selections[query_index], is.null, logical(1))]
  message("")
  message("Fuzzy matching with WoRMS ...")
  if (length(unresolved)) {
    fuzzy_records <- .wm_tidy_batches(
      lookup_names[unresolved], "fuzzy", batch_size,
      before_batch = function(index, batch_number, batch_count) {
        # Report original labels and positions, even when names share a lookup.
        input_index <- which(input$eval_taxa %in% lookup_names[unresolved[index]])
        for (i in input_index) {
          message(sprintf("%s %d/%d", input$input_taxa[i], i, nrow(input)))
        }
      }
    )
    for (j in seq_along(unresolved)) {
      i <- unresolved[j]
      selections[i] <- list(.wm_tidy_select(
        fuzzy_records[[j]], lookup_names[i], FALSE,
        prio_marine, prio_accepted, exclude_extinct
      ))
    }
  }

  # Build small result rows, then bind once rather than growing a data frame.
  accepted_cache <- new.env(parent = emptyenv())
  rows <- lapply(selections, .wm_tidy_extract, cache = accepted_cache)
  lookups <- tibble::tibble(eval_taxa = lookup_names) %>%
    dplyr::bind_cols(dplyr::bind_rows(rows))
  result <- input %>%
    dplyr::left_join(lookups, by = "eval_taxa", relationship = "many-to-one") %>%
    dplyr::mutate(flag = dplyr::case_when(
      .data$worms_match == "no_match" ~ 1L,
      stringr::str_detect(.data$worms_match, "multiple") ~ 1L,
      .data$eval_taxa != .data$valid_taxa ~ 1L,
      .data$match_aphia_id != .data$valid_aphia_id ~ 1L,
      is.na(.data$worms_match) | is.na(.data$valid_taxa) | is.na(.data$valid_aphia_id) ~ 1L,
      .data$match_rank != .data$valid_rank ~ 1L,
      .data$match_worms_status != "accepted" ~ 1L,
      TRUE ~ 0L
    )) %>%
    dplyr::select(dplyr::all_of(c("flag", "input_taxa", "eval_taxa",
                                 setdiff(names(template), "flag")))) %>%
    as.data.frame()
  message("")
  message("DONE")
  message(sprintf("Time difference of %.2f secs",
                  as.numeric(difftime(Sys.time(), started, units = "secs"))))
  message("Please double-check results")
  result
}

# Input checks -------------------------------------------------------------
.wm_tidy_validate <- function(taxa, eval_as, prio_marine, prio_accepted,
                              exclude_extinct, batch_size) {
  if (!is.character(taxa) || !is.null(dim(taxa)) || anyNA(taxa)) {
    stop("'taxa' must be a character vector without missing values.", call. = FALSE)
  }
  if (!is.null(eval_as) &&
      (!is.character(eval_as) || anyNA(eval_as) || is.null(names(eval_as)) ||
       anyNA(names(eval_as)) || any(!nzchar(names(eval_as))) ||
       anyDuplicated(names(eval_as)))) {
    stop("'eval_as' must be NULL or a uniquely named character vector without NAs.",
         call. = FALSE)
  }
  if (!all(vapply(list(prio_marine, prio_accepted, exclude_extinct), function(x) {
    is.logical(x) && length(x) == 1L && !is.na(x)
  }, logical(1)))) {
    stop("Priority and extinction arguments must be single TRUE/FALSE values.", call. = FALSE)
  }
  if (!is.numeric(batch_size) || length(batch_size) != 1L ||
      is.na(batch_size) || !is.finite(batch_size) || batch_size < 1 ||
      batch_size > 50 || batch_size != floor(batch_size)) {
    stop("'batch_size' must be an integer from 1 to 50.", call. = FALSE)
  }
}

# Preserve the original stringi encoding repair, but stop on unrepaired input.
.wm_tidy_utf8 <- function(taxa) {
  if (!all(stringi::stri_enc_isutf8(taxa))) {
    taxa <- stringi::stri_encode(taxa, from = "", to = "UTF-8")
  }
  if (anyNA(taxa) || !all(stringi::stri_enc_isutf8(taxa))) {
    stop("Convert 'taxa' to valid UTF-8 before matching.", call. = FALSE)
  }
  taxa
}

# API requests -------------------------------------------------------------
.wm_tidy_api <- function(method, value) {
  if (method == "exact") {
    return(worrms::wm_records_names(name = value, marine_only = TRUE))
  }
  # httr encodes query values, including non-ASCII names and reserved characters.
  if (method == "fuzzy") {
    query <- c(stats::setNames(as.list(value), rep("scientificnames[]", length(value))),
               list(marine_only = "false"))
    response <- httr::GET(
      "https://www.marinespecies.org/rest/AphiaRecordsByMatchNames",
      query = query, httr::timeout(60)
    )
  } else {
    response <- httr::GET(
      paste0("https://www.marinespecies.org/rest/AphiaRecordByAphiaID/", value),
      httr::timeout(60)
    )
  }
  httr::stop_for_status(response)
  if (httr::status_code(response) == 204L) {
    return(if (method == "fuzzy") vector("list", length(value)) else NULL)
  }
  jsonlite::fromJSON(httr::content(response, as = "text", encoding = "UTF-8"))
}

.wm_tidy_batches <- function(taxa, method, batch_size,
                              before_batch = NULL, after_batch = NULL) {
  records <- vector("list", length(taxa))
  if (!length(taxa)) return(records)
  starts <- seq.int(1L, length(taxa), by = batch_size)
  for (start in starts) {
    index <- seq.int(start, min(start + batch_size - 1L, length(taxa)))
    batch_number <- match(start, starts)
    if (!is.null(before_batch)) before_batch(index, batch_number, length(starts))
    response <- tryCatch({
      value <- .wm_tidy_api(method, taxa[index])
      if (!is.list(value) || is.data.frame(value) || length(value) != length(index)) {
        stop("Unexpected response shape; cannot align records with input names.")
      }
      value
    }, error = function(e) {
      warning(sprintf("WoRMS %s request failed: %s", method, conditionMessage(e)),
              call. = FALSE)
      NULL
    })
    if (is.null(response)) {
      response <- vector("list", length(index))
      if (length(index) > 1L) {
        for (j in seq_along(index)) {
          response[j] <- .wm_tidy_batches(taxa[index[j]], method, 1L)
        }
      }
    }
    records[index] <- response
    # Retry calls omit callbacks so they do not restart the visible batch count.
    if (!is.null(after_batch)) after_batch(index, batch_number, length(starts))
  }
  records
}

# Candidate selection ------------------------------------------------------
.wm_tidy_select <- function(records, name, exact, prio_marine,
                            prio_accepted, exclude_extinct) {
  if (!is.data.frame(records) || !nrow(records) ||
      !all(c("scientificname", "status") %in% names(records))) return(NULL)
  if (exact && nrow(records) >= 50L) return(NULL)
  candidates <- records %>%
    dplyr::filter(!is.na(.data$scientificname), !is.na(.data$status))
  if (exact) candidates <- candidates %>% dplyr::filter(.data$scientificname == name)
  n_candidates <- nrow(candidates)
  if (exclude_extinct && "isExtinct" %in% names(candidates)) {
    candidates <- candidates %>% dplyr::filter(is.na(.data$isExtinct) | .data$isExtinct == 0)
  }
  if (prio_marine && "isMarine" %in% names(candidates) && 1 %in% candidates$isMarine) {
    candidates <- candidates %>% dplyr::filter(.data$isMarine == 1)
  }
  if (prio_accepted && "accepted" %in% candidates$status) {
    candidates <- candidates %>% dplyr::filter(.data$status == "accepted")
  }
  if (!nrow(candidates)) return(NULL)
  selected <- candidates %>% dplyr::slice_head(n = 1L)
  type <- if (exact) "exact" else paste0("fuzzy_", .wm_tidy_field(selected, "match_type"))
  if (n_candidates > 1L) type <- paste0("multiple_", type)
  list(record = selected, type = type)
}

# Result extraction --------------------------------------------------------
.wm_tidy_field <- function(record, field) {
  value <- record[[field]]
  if (!length(value) || is.na(value[[1L]])) return(NA_character_)
  as.character(value[[1L]])
}

.wm_tidy_result_row <- function() {
  tibble::tibble(
    flag = 0L, match_taxa = NA_character_, worms_match = "no_match",
    match_worms_status = "no_match", match_aphia_id = NA_character_,
    match_rank = NA_character_, valid_taxa = NA_character_,
    valid_aphia_id = NA_character_, valid_rank = NA_character_,
    valid_phylum = NA_character_, valid_class = NA_character_,
    valid_order = NA_character_, valid_family = NA_character_,
    valid_genus = NA_character_, valid_species = NA_character_
  )
}

.wm_tidy_extract <- function(selection, cache) {
  result <- .wm_tidy_result_row()
  if (is.null(selection)) return(result)
  record <- selection$record
  result <- result %>% dplyr::mutate(
    match_taxa = .wm_tidy_field(record, "scientificname"),
    worms_match = selection$type,
    match_worms_status = .wm_tidy_field(record, "status"),
    match_aphia_id = .wm_tidy_field(record, "AphiaID"),
    match_rank = .wm_tidy_field(record, "rank"),
    valid_aphia_id = dplyr::coalesce(.wm_tidy_field(record, "valid_AphiaID"),
                                    .wm_tidy_field(record, "parentNameUsageID"))
  )
  id <- result$valid_aphia_id
  if (is.na(id)) {
    record <- NULL
  } else if (is.na(result$match_aphia_id) || id != result$match_aphia_id) {
    if (!exists(id, envir = cache, inherits = FALSE)) {
      accepted <- tryCatch(.wm_tidy_api("record", id), error = function(e) {
        warning(sprintf("Accepted-record lookup failed for AphiaID %s: %s",
                        id, conditionMessage(e)), call. = FALSE)
        NULL
      })
      assign(id, accepted, envir = cache)
    }
    record <- get(id, envir = cache, inherits = FALSE)
  }
  result %>% dplyr::mutate(
    valid_taxa = .wm_tidy_field(record, "valid_name"),
    valid_rank = .wm_tidy_field(record, "rank"),
    valid_phylum = .wm_tidy_field(record, "phylum"),
    valid_class = .wm_tidy_field(record, "class"),
    valid_order = .wm_tidy_field(record, "order"),
    valid_family = .wm_tidy_field(record, "family"),
    valid_genus = .wm_tidy_field(record, "genus"),
    valid_species = dplyr::if_else(.data$valid_rank == "Species", .data$valid_taxa, NA_character_)
  )
}

