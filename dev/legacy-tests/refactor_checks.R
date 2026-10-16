# Run from the repository root:
# Rscript --vanilla tests/refactor_checks.R
# Synthetic API records only; no network, installed packages, or user data.
local({
  code <- new.env(parent = baseenv())
  sys.source("R/worms_taxa_check_refactored.R", envir = code)
  stopifnot(identical(formals(code$wm_match)$prio_marine, TRUE))
  code$requireNamespace <- function(...) TRUE
  calls <- list()
  handler <- NULL
  code$.wm_api <- function(method, value) {
    calls[[length(calls) + 1L]] <<- list(method = method, value = value)
    handler(method, value)
  }
  record <- function(name, id = 1, valid_id = id, status = "accepted",
                     marine = 1, extinct = 0, rank = "Species") {
    data.frame(scientificname = name, AphiaID = id, valid_AphiaID = valid_id,
               parentNameUsageID = NA_real_, valid_name = name, status = status,
               isMarine = marine, isExtinct = extinct, rank = rank,
               phylum = "Examplephylum", class = "Exampleclass",
               order = "Exampleorder", family = "Examplefamily",
               genus = "Example", match_type = "near_1", stringsAsFactors = FALSE)
  }
  errors <- function(expr) inherits(tryCatch(force(expr), error = identity), "error")
  run <- function(...) suppressMessages(code$wm_match(...))
  warnings_seen <- character()
  capture_warnings <- function(expr) {
    warnings_seen <<- character()
    withCallingHandlers(expr, warning = function(w) {
      warnings_seen <<- c(warnings_seen, conditionMessage(w))
      invokeRestart("muffleWarning")
    })
  }

  # Stable empty output and early argument validation make no API calls.
  empty <- run(character())
  stopifnot(nrow(empty) == 0L, ncol(empty) == 17L,
            identical(empty$flag, integer()), is.character(empty$valid_aphia_id),
            errors(run(NA_character_)), errors(run(factor("Example"))),
            errors(run("Example", batch_size = 0)),
            errors(run("Example", batch_size = 1.5)),
            errors(run("Example", batch_size = 51)),
            errors(run("Example", prio_marine = NA)),
            errors(run("Example", eval_as = c("unnamed"))),
            errors(run("Example", eval_as = c(a = "x", a = "y"))),
            length(calls) == 0L)
  code$requireNamespace <- function(...) FALSE
  stopifnot(errors(run("Example")))
  code$requireNamespace <- function(...) TRUE

  # Identical cleaned names share requests; original labels and order survive.
  handler <- function(method, value) lapply(value, record)
  matched <- run(c(" Example alpha sp. ", "Example alpha", "Example alpha",
                   "Other", "cfname"), eval_as = c(Other = "Example alpha"))
  stopifnot(nrow(matched) == 4L, length(calls) == 1L,
            identical(calls[[1L]]$value, c("Example alpha", "cfname")),
            identical(matched$flag, rep(0L, 4)),
            identical(matched$valid_taxa,
                      c("Example alpha", "Example alpha", "Example alpha", "cfname")))

  # Positional empty entries must never shift later matches; fuzzy calls batch.
  calls <- list()
  handler <- function(method, value) {
    if (method == "exact") return(lapply(value, function(x) {
      if (x == "Exact") record(x) else NULL
    }))
    lapply(value, function(x) if (x == "Miss") NULL else record("Corrected"))
  }
  fuzzy <- run(c("Typo1", "Exact", "Typo2", "Miss", "!!!"), batch_size = 2)
  stopifnot(identical(fuzzy$worms_match,
                      c("fuzzy_near_1", "exact", "fuzzy_near_1", "no_match", "no_match")),
            identical(fuzzy$flag, c(1L, 0L, 1L, 1L, 1L)),
            length(calls) == 4L,
            !any(vapply(calls, function(x) any(x$value == ""), logical(1))))

  # Priority order, unknown ecology, extinct exclusion, and multiple flags.
  choices <- rbind(record("Choice", 1, marine = 0),
                   record("Choice", 2, marine = 1, status = "unaccepted"),
                   record("Choice", 3, marine = 1),
                   record("Choice", 4, marine = 1, extinct = 1))
  handler <- function(method, value) lapply(value, function(x) choices)
  stopifnot(run("Choice")$match_aphia_id == "3",
            run("Choice", prio_marine = FALSE)$match_aphia_id == "1",
            run("Choice", prio_accepted = FALSE)$match_aphia_id == "2",
            run("Choice")$worms_match == "multiple_exact",
            run("Choice")$flag == 1L)
  choices <- record("Unknown", marine = NA, extinct = NA)
  stopifnot(run("Unknown")$flag == 0L)
  choices <- record("Extinct", extinct = 1)
  stopifnot(run("Extinct")$worms_match == "no_match",
            run("Extinct", exclude_extinct = FALSE)$worms_match == "exact")

  # A saturated exact result must take the fuzzy path.
  handler <- function(method, value) lapply(value, function(x) {
    if (method == "exact") record(x)[rep(1L, 50), ] else record(x)
  })
  stopifnot(run("Saturated")$worms_match == "fuzzy_near_1")

  # Synonyms share a cached accepted lookup, also when that lookup fails.
  calls <- list()
  handler <- function(method, value) {
    if (method == "record") return(record("Accepted", 99))
    lapply(value, function(x) record(x, valid_id = 99, status = "unaccepted"))
  }
  synonyms <- run(c("Synonym1", "Synonym2"))
  stopifnot(identical(synonyms$valid_taxa, rep("Accepted", 2)),
            length(calls) == 2L, all(synonyms$flag == 1L))
  handler <- function(method, value) {
    if (method == "record") stop("synthetic service failure")
    lapply(value, function(x) record(x, valid_id = 99))
  }
  failed <- capture_warnings(run(c("Synonym1", "Synonym2")))
  stopifnot(all(is.na(failed$valid_taxa)), all(failed$flag == 1L),
            length(warnings_seen) == 1L)

  # Parent fallback and incomplete API fields remain explicit.
  handler <- function(method, value) {
    if (method == "record") return(record("Parent", 99, rank = "Genus"))
    lapply(value, function(x) {
      answer <- record(x, valid_id = NA)
      answer$parentNameUsageID <- 99
      answer
    })
  }
  parent <- run("Child")
  stopifnot(parent$valid_aphia_id == "99", parent$valid_rank == "Genus",
            is.na(parent$valid_species), parent$flag == 1L)
  handler <- function(method, value) lapply(value, function(x) {
    answer <- record(x)
    answer$family <- NULL
    answer
  })
  stopifnot(is.na(run("Incomplete")$valid_family))

  # Batch failure (or shape mismatch) falls back to individual names.
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
    stopifnot(all(recovered$flag == 0L), length(calls) == 3L,
              length(warnings_seen) == 1L)
  }
  handler <- function(...) stop("synthetic outage")
  outage <- capture_warnings(run("Unavailable"))
  stopifnot(outage$worms_match == "no_match", outage$flag == 1L,
            length(warnings_seen) == 2L)

  # Join preserves repeated rows, exact whitespace labels, and factor ordering.
  handler <- function(method, value) lapply(value, record)
  validation <- run(c(" Example ", "Other"))
  observations <- data.frame(label = c(" Example ", "Missing", " Example "),
                             group = factor(c("b", "a", "b"), levels = c("b", "a")),
                             species = "stale", rank = "stale", aphia_id = "stale")
  joined <- code$add_wm_taxonomy(observations, validation, "label")
  stopifnot(nrow(joined) == nrow(observations),
            identical(joined$original_taxa, observations$label),
            identical(joined$taxa, c("Example", "Missing", "Example")),
            identical(joined$group, observations$group),
            !anyDuplicated(names(joined)),
            errors(code$add_wm_taxonomy(observations, rbind(validation, validation), "label")),
            errors(code$add_wm_taxonomy(observations, validation, "absent")),
            nrow(code$add_wm_taxonomy(observations[FALSE, ], validation, "label")) == 0L)
  cat("All refactor checks passed (synthetic API; no live WoRMS requests).\n")
})
