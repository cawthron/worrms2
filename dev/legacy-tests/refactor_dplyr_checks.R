# Run from the repository root in the renv-enabled session:
# source("tests/refactor_dplyr_checks.R")
# Synthetic records only: no live requests or input/result files are written.
local({
  code <- new.env(parent = globalenv())
  original <- new.env(parent = globalenv())
  suppressPackageStartupMessages({
    sys.source("R/worms_taxa_check_refactored_dplyr.R", envir = code)
    sys.source("R/worms_taxa_check_FUNCTION_v7.5.R", envir = original)
  })
  stopifnot(identical(formals(code$wm_match3), formals(original$wm_match)),
            identical(formals(code$wm_match3)$prio_marine, TRUE))
  calls <- list()
  handler <- NULL
  code$.wm_tidy_api <- function(method, value) {
    calls[[length(calls) + 1L]] <<- list(method = method, value = value)
    handler(method, value)
  }
  record <- function(name, id = 1, valid_id = id, status = "accepted",
                     marine = 1, extinct = 0, rank = "Species") {
    tibble::tibble(scientificname = name, AphiaID = id, valid_AphiaID = valid_id,
                   parentNameUsageID = NA_real_, valid_name = name, status = status,
                   isMarine = marine, isExtinct = extinct, rank = rank,
                   phylum = "Examplephylum", class = "Exampleclass",
                   order = "Exampleorder", family = "Examplefamily",
                   genus = "Example", match_type = "near_1")
  }
  run <- function(...) suppressMessages(code$wm_match3(...))
  errors <- function(expr) inherits(tryCatch(force(expr), error = identity), "error")
  warnings_seen <- character()
  capture_warnings <- function(expr) {
    warnings_seen <<- character()
    withCallingHandlers(expr, warning = function(w) {
      warnings_seen <<- c(warnings_seen, conditionMessage(w))
      invokeRestart("muffleWarning")
    })
  }

  # Compare against the original on deterministic exact, synonym, and fuzzy
  # responses, using the original HTTP boundaries without making requests.
  handler <- function(method, value) {
    if (method == "record") return(record("Accepted", 99))
    lapply(value, function(name) {
      if (name == "Typo") {
        if (method == "exact") return(NULL)
        return(record("Corrected"))
      }
      if (name == "Synonym") return(record(name, valid_id = 99, status = "unaccepted"))
      record(name)
    })
  }
  original$wm_records_names <- function(name) handler("exact", name)
  original$GET <- function(url, ...) {
    if (grepl("AphiaRecordByAphiaID", url)) return(handler("record", 99))
    name <- sub(".*scientificnames\\[\\]=([^&]*).*", "\\1", utils::URLdecode(url))
    handler("fuzzy", name)
  }
  original$content <- function(resp, ...) resp
  original$fromJSON <- function(txt) txt
  baseline_inputs <- c(" Exact alpha sp. ", "Exact beta", "Synonym", "Typo", "Override")
  baseline_overrides <- c(Override = "Exact beta")
  baseline <- suppressMessages(capture.output({
    expected <- original$wm_match(baseline_inputs, eval_as = baseline_overrides)
  }))
  actual <- run(baseline_inputs, eval_as = baseline_overrides)
  stopifnot(isTRUE(all.equal(actual, expected, check.attributes = FALSE)))

  # Input validation and a typed zero-row result do not query the service.
  calls <- list()
  empty <- run(character())
  stopifnot(nrow(empty) == 0L, ncol(empty) == 17L,
            identical(names(empty), names(actual)), is.character(empty$valid_aphia_id),
            errors(run(NA_character_)), errors(run(1)),
            errors(run("One", batch_size = 0)), errors(run("One", batch_size = 51)),
            errors(run("One", prio_marine = NA)),
            errors(run("One", eval_as = c("unnamed"))), length(calls) == 0L)

  # Share lookup results across overrides, but preserve each original label.
  handler <- function(method, value) lapply(value, record)
  shared <- run(c("Same sp.", "Same", "Other", "Same"), eval_as = c(Other = "Same"))
  stopifnot(nrow(shared) == 3L, length(calls) == 1L,
            identical(calls[[1L]]$value, "Same"), all(shared$flag == 0L))

  # Preserve empty slots and batch unresolved names only; do not query blanks.
  calls <- list()
  handler <- function(method, value) lapply(value, function(name) {
    if (name == "Missing") return(NULL)
    if (method == "exact" && name != "Exact") return(NULL)
    record(if (name == "Exact") name else "Corrected")
  })
  fuzzy <- run(c("Typo1", "Exact", "Typo2", "Missing", "!!!"), batch_size = 2)
  stopifnot(identical(fuzzy$worms_match,
                      c("fuzzy_near_1", "exact", "fuzzy_near_1", "no_match", "no_match")),
            identical(fuzzy$flag, c(1L, 0L, 1L, 1L, 1L)), length(calls) == 4L)

  # Progress uses original labels/positions, including shared cleaned names.
  progress <- character()
  withCallingHandlers(
    invisible(code$wm_match3(c("Typo1", "Exact", "Typo2", "Typo1 sp.", "Exact2"),
                             batch_size = 2)),
    message = function(m) {
      progress <<- c(progress, sub("\n$", "", conditionMessage(m)))
      invokeRestart("muffleMessage")
    }
  )
  elapsed_index <- grep("^Time difference of [0-9]+\\.[0-9]{2} secs$", progress)
  stopifnot(length(elapsed_index) == 1L,
            identical(progress[-elapsed_index], c(
    "Direct matching with WoRMS ...", "Batch 1/2", "Batch 2/2", "",
    "Fuzzy matching with WoRMS ...", "Typo1 1/5", "Typo2 3/5",
    "Typo1 sp. 4/5", "Exact2 5/5", "", "DONE", "Please double-check results"
  )))
  # The fuzzy heading and DONE also appear when all names match directly.
  handler <- function(method, value) lapply(value, record)
  progress <- character()
  withCallingHandlers(invisible(code$wm_match3("Exact")), message = function(m) {
    progress <<- c(progress, sub("\n$", "", conditionMessage(m)))
    invokeRestart("muffleMessage")
  })
  elapsed_index <- grep("^Time difference of [0-9]+\\.[0-9]{2} secs$", progress)
  stopifnot(length(elapsed_index) == 1L,
            identical(progress[-elapsed_index], c("Direct matching with WoRMS ...", "Batch 1/1", "",
                                "Fuzzy matching with WoRMS ...", "", "DONE",
                                "Please double-check results")))

  # Exclusion precedes marine and accepted preferences; retain multiple labels.
  choices <- dplyr::bind_rows(record("Choice", 1, marine = 0),
                              record("Choice", 2, status = "unaccepted"),
                              record("Choice", 3), record("Choice", 4, extinct = 1))
  handler <- function(method, value) lapply(value, function(name) choices)
  stopifnot(run("Choice")$match_aphia_id == "3",
            run("Choice", prio_marine = FALSE)$match_aphia_id == "1",
            run("Choice", prio_accepted = FALSE)$match_aphia_id == "2",
            run("Choice")$worms_match == "multiple_exact", run("Choice")$flag == 1L)
  choices <- record("Unknown", marine = NA, extinct = NA)
  stopifnot(run("Unknown")$flag == 0L)
  choices <- record("Extinct", extinct = 1)
  stopifnot(run("Extinct")$worms_match == "no_match",
            run("Extinct", exclude_extinct = FALSE)$worms_match == "exact")
  handler <- function(method, value) lapply(value, function(name) {
    if (method == "exact") record(name)[rep(1L, 50), ] else record(name)
  })
  stopifnot(run("Saturated")$worms_match == "fuzzy_near_1")

  # Cache accepted records and failures by ID; never reuse synonym taxonomy.
  calls <- list()
  handler <- function(method, value) {
    if (method == "record") return(record("Accepted", 99))
    lapply(value, function(name) record(name, valid_id = 99, status = "unaccepted"))
  }
  synonyms <- run(c("Synonym1", "Synonym2"))
  stopifnot(identical(synonyms$valid_taxa, rep("Accepted", 2)), length(calls) == 2L)
  handler <- function(method, value) {
    if (method == "record") stop("synthetic accepted-record failure")
    lapply(value, function(name) record(name, valid_id = 99))
  }
  failed <- capture_warnings(run(c("Synonym1", "Synonym2")))
  stopifnot(all(is.na(failed$valid_taxa)), all(failed$flag == 1L), length(warnings_seen) == 1L)

  # Batch failures or malformed outer response shapes retry individual names.
  for (bad_shape in c(FALSE, TRUE)) {
    calls <- list()
    handler <- function(method, value) {
      if (length(value) > 1L) {
        if (bad_shape) return(list(NULL))
        stop("synthetic batch failure")
      }
      lapply(value, record)
    }
    recovered <- capture_warnings(run(c("One", "Two")))
    stopifnot(all(recovered$flag == 0L), length(calls) == 3L, length(warnings_seen) == 1L)
  }
  handler <- function(...) stop("synthetic outage")
  outage <- capture_warnings(run("Unavailable"))
  stopifnot(outage$worms_match == "no_match", outage$flag == 1L, length(warnings_seen) == 2L)

  # Exact original labels, repeated rows, and factor levels survive the join.
  handler <- function(method, value) lapply(value, record)
  validation <- run(c(" Example ", "Other"))
  observations <- tibble::tibble(label = c(" Example ", "Missing", " Example "),
                                  group = factor(c("b", "a", "b"), levels = c("b", "a")),
                                  species = "stale", rank = "stale", aphia_id = "stale")
  joined <- code$add_wm_taxonomy(observations, validation, "label")
  stopifnot(nrow(joined) == 3L, identical(joined$original_taxa, observations$label),
            identical(joined$taxa, c("Example", "Missing", "Example")),
            identical(joined$group, observations$group), !anyDuplicated(names(joined)),
            errors(code$add_wm_taxonomy(observations, dplyr::bind_rows(validation, validation), "label")),
            nrow(code$add_wm_taxonomy(observations[FALSE, ], validation, "label")) == 0L)
  cat("All dplyr refactor checks passed, including comparison with the original.\n")
})
