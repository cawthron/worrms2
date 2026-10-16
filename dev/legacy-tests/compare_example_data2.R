# Explicit live comparison from a project R session:
# Sys.setenv(WORRMS2_RUN_COMPARISON = "true")
# source("tests/compare_example_data2.R")
# Results and API responses are kept in ignored renv/local/ for diagnosis.
if (identical(Sys.getenv("WORRMS2_RUN_COMPARISON"), "true")) local({
  if (.Platform$OS.type == "windows") {
    invisible(Sys.setlocale("LC_CTYPE", ".UTF-8"))
  }
  example <- withCallingHandlers(
    read.csv("data/example_data2.csv", fileEncoding = "UTF-8-BOM",
             stringsAsFactors = FALSE, na.strings = c("", "NA")),
    warning = function(w) stop(conditionMessage(w), call. = FALSE)
  )
  stopifnot("taxa" %in% names(example), !anyNA(example$taxa))
  original <- new.env(parent = globalenv())
  refactored <- new.env(parent = globalenv())
  suppressPackageStartupMessages({
    sys.source("R/worms_taxa_check_FUNCTION_v7.5.R", envir = original)
    sys.source("R/worms_taxa_check_refactored_dplyr.R", envir = refactored)
  })
  stopifnot(identical(formals(original$wm_match), formals(refactored$wm_match3)))
  requests <- list(original = list(), refactored = list())
  warnings_seen <- list(original = character(), refactored = character())
  timings <- numeric()
  results <- list()

  # Observe API boundaries without changing matching logic or response content.
  original$wm_records_names <- function(name) {
    response <- worrms::wm_records_names(name = name)
    requests$original[[length(requests$original) + 1L]] <<-
      list(method = "exact", value = name, response = response)
    response
  }
  original$GET <- function(url, ...) {
    response <- httr::GET(url, ..., httr::timeout(60))
    method <- if (grepl("AphiaRecordsByMatchNames", url)) "fuzzy" else "record"
    value <- if (method == "fuzzy") {
      httr::parse_url(url)$query[["scientificnames[]"]]
    } else sub(".*/", "", url)
    requests$original[[length(requests$original) + 1L]] <<-
      list(method = method, value = value, response = response)
    response
  }
  tidy_api <- refactored$.wm_tidy_api
  refactored$.wm_tidy_api <- function(method, value) {
    response <- tidy_api(method, value)
    requests$refactored[[length(requests$refactored) + 1L]] <<-
      list(method = method, value = value, response = response)
    response
  }

  for (version in c("original", "refactored")) {
    cat("\nRunning", version, "on", nrow(example), "example taxa\n")
    elapsed <- system.time({
      results[[version]] <- withCallingHandlers(
        if (version == "original") original$wm_match(example$taxa)
        else refactored$wm_match3(example$taxa),
        warning = function(w) {
          warnings_seen[[version]] <<- c(warnings_seen[[version]], conditionMessage(w))
        }
      )
    })[["elapsed"]]
    timings[version] <- elapsed
    # Checkpoint the first run so a later service failure does not lose it.
    dir.create("renv/local", showWarnings = FALSE, recursive = TRUE)
    saveRDS(list(results = results, timings = timings, requests = requests,
                 warnings = warnings_seen, utc = format(Sys.time(), tz = "UTC")),
            "renv/local/example_data2_comparison.rds")
  }

  stopifnot(identical(names(results$original), names(results$refactored)),
            !anyDuplicated(results$original$input_taxa),
            !anyDuplicated(results$refactored$input_taxa),
            setequal(results$original$input_taxa, results$refactored$input_taxa))
  aligned <- results$refactored[match(results$original$input_taxa,
                                     results$refactored$input_taxa), , drop = FALSE]
  differences <- dplyr::bind_rows(lapply(names(results$original), function(column) {
    before <- as.character(results$original[[column]])
    after <- as.character(aligned[[column]])
    changed <- is.na(before) != is.na(after) |
      (!is.na(before) & !is.na(after) & before != after)
    tibble::tibble(input_taxa = results$original$input_taxa[changed],
                   column = column, original = before[changed], refactored = after[changed])
  }))
  types <- tibble::tibble(
    column = names(results$original),
    original = vapply(results$original, typeof, character(1)),
    refactored = vapply(aligned, typeof, character(1))
  ) %>% dplyr::filter(original != refactored)
  attribute_differences <- dplyr::bind_rows(lapply(names(results$original), function(column) {
    before <- attributes(results$original[[column]])
    after <- attributes(aligned[[column]])
    attribute_names <- union(names(before), names(after))
    changed <- attribute_names[!vapply(attribute_names, function(attribute) {
      identical(before[[attribute]], after[[attribute]])
    }, logical(1))]
    tibble::tibble(column = rep(column, length(changed)), attribute = changed)
  }))
  summary <- dplyr::bind_rows(lapply(names(results), function(version) {
    result <- results[[version]]
    tibble::tibble(version = version, rows = nrow(result),
                   matched = sum(result$worms_match != "no_match", na.rm = TRUE),
                   unmatched = sum(result$worms_match == "no_match", na.rm = TRUE),
                   missing_match_type = sum(is.na(result$worms_match)),
                   flagged = sum(result$flag == 1L, na.rm = TRUE),
                   seconds = unname(timings[version]),
                   requests = length(requests[[version]]),
                   warnings = length(warnings_seen[[version]]))
  }))
  comparison <- list(results = results, timings = timings, requests = requests,
                     warnings = warnings_seen, summary = summary,
                     differences = differences, types = types,
                     attribute_differences = attribute_differences,
                     same_row_order = identical(results$original$input_taxa,
                                                results$refactored$input_taxa),
                     utc = format(Sys.time(), tz = "UTC"),
                     versions = c(R = as.character(getRversion()),
                                  worrms = as.character(utils::packageVersion("worrms"))))
  saveRDS(comparison, "renv/local/example_data2_comparison.rds")
  cat("\nComparison summary\n")
  print(summary, width = Inf)
  cat("Different cells:", nrow(differences), "\n")
  cat("Taxa with differences:", dplyr::n_distinct(differences$input_taxa), "\n")
  print(differences, n = Inf, width = Inf)
  cat("Column type differences\n")
  print(types, n = Inf)
  cat("Column attribute differences\n")
  print(attribute_differences, n = Inf)
})
