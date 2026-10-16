# Run explicitly from the repository root in an renv-enabled R session:
# Sys.setenv(WORRMS2_RUN_LIVE_TESTS = "true")
# source("dev/checks/live_worms_check.R")
# Install the package first; WORRMS2_EXAMPLE_FILE can select example_data2.csv.
# Uses live WoRMS requests; rerun results can change with the database.
# No raw input or taxonomic result tables are written to disk.
# Opt in so ordinary test runs and future package checks do not use the network.
if (identical(Sys.getenv("WORRMS2_RUN_LIVE_TESTS"), "true")) local({
  # Windows may start in the C locale when an inherited C.UTF-8 is unavailable.
  # Set the character locale before importing the UTF-8 CSV with its BOM.
  old_locale <- Sys.getlocale("LC_CTYPE")
  on.exit(Sys.setlocale("LC_CTYPE", old_locale), add = TRUE)
  if (.Platform$OS.type == "windows") {
    if (!nzchar(Sys.setlocale("LC_CTYPE", ".UTF-8"))) {
      stop("A UTF-8 character locale is required to import the example CSV.")
    }
  }
  code <- asNamespace("worrms2")
  stopifnot(identical(formals(code$wm_match)$prio_marine, TRUE))
  example_file <- match.arg(Sys.getenv("WORRMS2_EXAMPLE_FILE", "example_data.csv"),
                            c("example_data.csv", "example_data2.csv"))

  # Promote import warnings to errors so partial reads cannot pass unnoticed.
  example <- withCallingHandlers(
    read.csv(system.file("extdata", example_file, package = "worrms2", mustWork = TRUE),
             fileEncoding = "UTF-8-BOM",
             stringsAsFactors = FALSE, na.strings = c("", "NA")),
    warning = function(w) stop(conditionMessage(w), call. = FALSE)
  )
  stopifnot("taxa" %in% names(example),
            nrow(example) > 0L, !anyNA(example$taxa))
  if (!"eval_as" %in% names(example)) example$eval_as <- NA_character_
  has_override <- !is.na(example$eval_as) & nzchar(example$eval_as)
  overrides <- stats::setNames(example$eval_as[has_override],
                               example$taxa[has_override])
  if (!length(overrides)) overrides <- NULL

  # Any API warning fails this smoke test: service failures must not pass as
  # ordinary no-matches. Actual unmatched taxa remain valid review outcomes.
  elapsed <- system.time({
    validation <- withCallingHandlers(
      code$wm_match(example$taxa, eval_as = overrides),
      warning = function(w) stop(conditionMessage(w), call. = FALSE)
    )
  })[["elapsed"]]
  enriched <- code$add_wm_taxonomy(example, validation)
  matched <- validation$worms_match != "no_match"
  stopifnot(
    nrow(validation) == length(unique(example$taxa)), ncol(validation) == 17L,
    identical(validation$input_taxa, unique(example$taxa)),
    all(validation$flag %in% c(0L, 1L)), !anyNA(validation$worms_match),
    any(matched), all(!is.na(validation$match_aphia_id[matched])),
    all(!is.na(validation$valid_taxa[matched])),
    all(!is.na(validation$valid_aphia_id[matched])),
    all(validation$flag[!matched] == 1L),
    nrow(enriched) == nrow(example),
    identical(enriched$original_taxa, example$taxa),
    identical(validation$eval_taxa[match(example$taxa[has_override],
                                        validation$input_taxa)],
              unname(overrides))
  )
  cat("Live WoRMS check passed\n")
  cat("UTC time:", format(Sys.time(), tz = "UTC", usetz = TRUE), "\n")
  cat("R:", as.character(getRversion()), "\n")
  cat("worrms:", as.character(utils::packageVersion("worrms")), "\n")
  cat("Example rows:", nrow(example), "\n")
  cat("Overrides:", sum(has_override), "\n")
  cat("Matched:", sum(matched), "\n")
  cat("Unmatched:", sum(!matched), "\n")
  cat("Flagged for review:", sum(validation$flag == 1L), "\n")
  cat("Elapsed seconds:", elapsed, "\n")
  print(table(validation$worms_match))
})
