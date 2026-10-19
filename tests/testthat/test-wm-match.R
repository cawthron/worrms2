# Mock the original client's boundaries; ordinary tests never contact WoRMS.
test_that("the matcher rejects missing names before requests", {
  local_mocked_bindings(
    wm_records_names = function(...) stop("Unexpected API call"),
    GET = function(...) stop("Unexpected HTTP call"), .package = "worrms2"
  )
  expect_identical(formals(wm_match)$prio_marine, TRUE)
  expect_equal(formals(wm_match)$batch_size, 49)
  expect_error(quiet_match(NA_character_), "Taxa should not contain any NAs")
})

test_that("exact batches preserve distinct original inputs and output columns", {
  calls <- list()
  local_mocked_bindings(
    wm_records_names = function(name) {
      calls[[length(calls) + 1L]] <<- as.character(name)
      lapply(name, example_record)
    },
    GET = function(...) stop("Unexpected HTTP call"), .package = "worrms2"
  )
  result <- quiet_match(c("Same sp.", "Same", "Other", "Same"),
                        eval_as = c(Other = "Same"), batch_size = 2)
  expect_identical(result$input_taxa, c("Same sp.", "Same", "Other"))
  expect_identical(result$eval_taxa, rep("Same", 3))
  expect_identical(calls, list(c("Same", "Same"), "Same"))
  expect_equal(result$flag, c(1, 0, 1))
  expect_identical(result$match_aphia_id, rep("1", 3))
  expect_identical(names(result), c(
    "flag", "input_taxa", "eval_taxa", "match_taxa", "worms_match",
    "match_worms_status", "match_aphia_id", "match_rank", "valid_taxa",
    "valid_aphia_id", "valid_rank", "valid_phylum", "valid_class",
    "valid_order", "valid_family", "valid_genus", "valid_species"
  ))
})

test_that("candidate preferences preserve order and multiple-match labels", {
  choices <- rbind(example_record("Choice", 1, marine = 0),
                   example_record("Choice", 2, status = "unaccepted"),
                   example_record("Choice", 3),
                   example_record("Choice", 4, extinct = 1))
  local_mocked_bindings(
    wm_records_names = function(name) lapply(name, function(x) choices),
    GET = function(...) stop("Unexpected HTTP call"), .package = "worrms2"
  )
  expect_identical(quiet_match("Choice")$match_aphia_id, "3")
  expect_identical(quiet_match("Choice", prio_marine = FALSE)$match_aphia_id, "1")
  expect_identical(quiet_match("Choice", prio_accepted = FALSE)$match_aphia_id, "2")
  expect_equal(quiet_match("Choice", prio_accepted = FALSE)$flag, 2)
  expect_identical(quiet_match("Choice")$worms_match, "multiple_exact")
  expect_equal(quiet_match("Choice")$flag, 1)
})

test_that("missing and saturated exact results use individual fuzzy requests", {
  urls <- character()
  local_mocked_bindings(
    wm_records_names = function(name) lapply(name, function(x) {
      if (x == "Saturated") return(example_record(x)[rep(1L, 50), ])
      if (x == "Exact") return(example_record(x))
      NULL
    }),
    GET = function(url) {
      urls <<- c(urls, url)
      jsonlite::toJSON(list(example_record("Corrected")), dataframe = "rows")
    },
    content = function(x, ...) x, .package = "worrms2"
  )
  result <- quiet_match(c("Typo1", "Exact", "Typo2", "Saturated"))
  expect_identical(result$worms_match,
                   c("fuzzy_near_1", "exact", "fuzzy_near_1", "fuzzy_near_1"))
  expect_equal(result$flag, c(1, 0, 1, 1))
  expect_length(urls, 3)
  expect_true(all(grepl("marine_only=false", urls, fixed = TRUE)))
})

test_that("synonyms fetch accepted records individually and receive flag 2", {
  requests <- 0L
  local_mocked_bindings(
    wm_records_names = function(name) lapply(name, function(x) {
      example_record(x, valid_id = 99, status = "unaccepted")
    }),
    GET = function(url) {
      requests <<- requests + 1L
      expect_match(url, "AphiaRecordByAphiaID/99", fixed = TRUE)
      jsonlite::toJSON(as.list(example_record("Accepted", 99)), auto_unbox = TRUE)
    },
    content = function(x, ...) x, .package = "worrms2"
  )
  result <- quiet_match(c("Synonym1", "Synonym2"))
  expect_equal(requests, 2)
  expect_identical(result$valid_taxa, rep("Accepted", 2))
  expect_identical(result$valid_aphia_id, rep("99", 2))
  expect_equal(result$flag, c(2, 2))
})

test_that("missing valid identifiers retrieve parent taxonomy", {
  local_mocked_bindings(
    wm_records_names = function(name) lapply(name, function(x) {
      record <- example_record(x, valid_id = NA)
      record$parentNameUsageID <- 99
      record
    }),
    GET = function(url) {
      jsonlite::toJSON(as.list(example_record("Parent", 99, rank = "Genus")),
                       auto_unbox = TRUE)
    },
    content = function(x, ...) x, .package = "worrms2"
  )
  result <- quiet_match("Child")
  expect_identical(result$valid_aphia_id, "99")
  expect_identical(result$valid_rank, "Genus")
  expect_true(is.na(result$valid_species))
  expect_equal(result$flag, 2)
})

test_that("original batch and individual error behaviour is preserved", {
  local_mocked_bindings(
    wm_records_names = function(...) stop("Synthetic batch failure"),
    GET = function(...) stop("Unexpected HTTP call"), .package = "worrms2"
  )
  expect_error(quiet_match(c("One", "Two")), "Synthetic batch failure")
  local_mocked_bindings(
    wm_records_names = function(name) lapply(name, function(x) NULL),
    GET = function(...) stop("Synthetic lookup failure"), .package = "worrms2"
  )
  missing <- quiet_match("Missing")
  expect_identical(missing$worms_match, "no_match")
  expect_equal(missing$flag, 2)
  local_mocked_bindings(
    wm_records_names = function(name) lapply(name, function(x) {
      example_record(x, valid_id = 99)
    }), .package = "worrms2"
  )
  failed <- quiet_match("Synonym")
  expect_identical(failed$valid_taxa, "Synonym")
  expect_identical(failed$valid_aphia_id, "99")
  expect_equal(failed$flag, 2)
})

test_that("progress reports exact batches, fuzzy positions, timing and review", {
  local_mocked_bindings(
    wm_records_names = function(name) lapply(name, function(x) {
      if (x == "Exact") example_record(x) else NULL
    }),
    GET = function(url) jsonlite::toJSON(list(example_record("Corrected")),
                                        dataframe = "rows"),
    content = function(x, ...) x, .package = "worrms2"
  )
  progress <- character()
  timing <- capture.output(withCallingHandlers(
    invisible(wm_match(c("Typo1", "Exact", "Typo2"), batch_size = 2)),
    message = function(m) {
      progress <<- c(progress, sub("\n$", "", conditionMessage(m)))
      invokeRestart("muffleMessage")
    }
  ))
  expect_true(any(grepl("Time difference of", timing, fixed = TRUE)))
  expect_identical(progress, c(
    "Direct matching with WoRMS ...", "Batch 1/2", "Batch 2/2", "",
    "Fuzzy matching with WoRMS ...", "Typo1 1/3", "Typo2 3/3", "", "DONE",
    "Please doublecheck results"
  ))
})
