test_that("invalid and empty inputs never call the API", {
  local_mocked_bindings(.wm_tidy_api = function(...) stop("Unexpected API call"),
                        .package = "worrms2")
  expect_identical(formals(wm_match)$prio_marine, TRUE)
  empty <- quiet_match(character())
  expect_s3_class(empty, "data.frame")
  expect_equal(dim(empty), c(0L, 17L))
  expect_type(empty$valid_aphia_id, "character")
  expect_type(empty$flag, "integer")
  expect_error(quiet_match(NA_character_), "without missing")
  expect_error(quiet_match(1), "character vector")
  for (size in list(0, 51, 1.5, NA, Inf)) {
    expect_error(quiet_match("Example", batch_size = size), "integer from 1 to 50")
  }
  expect_error(quiet_match("Example", prio_marine = NA), "TRUE/FALSE")
  expect_error(quiet_match("Example", eval_as = "unnamed"), "uniquely named")
  expect_error(quiet_match("Example", eval_as = c(a = "x", a = "y")), "uniquely named")
})

test_that("cleaned names share lookups and preserve original order", {
  calls <- list()
  local_mocked_bindings(.wm_tidy_api = function(method, value) {
    calls[[length(calls) + 1L]] <<- list(method = method, value = value)
    lapply(value, example_record)
  }, .package = "worrms2")
  inputs <- c("Same sp.", "Same", "Other", "Same")
  result <- quiet_match(inputs, eval_as = c(Other = "Same"))
  expect_identical(result$input_taxa, unique(inputs))
  expect_identical(result$eval_taxa, rep("Same", 3))
  expect_identical(result$flag, rep(0L, 3))
  expect_length(calls, 1)
  expect_identical(calls[[1]]$value, "Same")
})

test_that("fuzzy batches preserve NULL slots and skip empty names", {
  calls <- list()
  local_mocked_bindings(.wm_tidy_api = function(method, value) {
    calls[[length(calls) + 1L]] <<- list(method = method, value = value)
    lapply(value, function(name) {
      if (name == "Missing" || (method == "exact" && name != "Exact")) return(NULL)
      example_record(if (name == "Exact") name else "Corrected")
    })
  }, .package = "worrms2")
  result <- quiet_match(c("Typo1", "Exact", "Typo2", "Missing", "!!!"), batch_size = 2)
  expect_identical(result$worms_match,
                    c("fuzzy_near_1", "exact", "fuzzy_near_1", "no_match", "no_match"))
  expect_identical(result$flag, c(1L, 0L, 1L, 1L, 1L))
  expect_length(calls, 4)
  expect_false(any(vapply(calls, function(call) any(call$value == ""), logical(1))))
})

test_that("candidate preferences retain their original order and multiple flags", {
  choices <- rbind(example_record("Choice", 1, marine = 0),
                    example_record("Choice", 2, status = "unaccepted"),
                    example_record("Choice", 3), example_record("Choice", 4, extinct = 1))
  local_mocked_bindings(.wm_tidy_api = function(method, value) {
    lapply(value, function(name) choices)
  }, .package = "worrms2")
  expect_identical(quiet_match("Choice")$match_aphia_id, "3")
  expect_identical(quiet_match("Choice", prio_marine = FALSE)$match_aphia_id, "1")
  expect_identical(quiet_match("Choice", prio_accepted = FALSE)$match_aphia_id, "2")
  expect_identical(quiet_match("Choice")$worms_match, "multiple_exact")
  expect_identical(quiet_match("Choice")$flag, 1L)
  choices <- example_record("Unknown", marine = NA, extinct = NA)
  expect_identical(quiet_match("Unknown")$flag, 0L)
  choices <- example_record("Extinct", extinct = 1)
  expect_identical(quiet_match("Extinct")$worms_match, "no_match")
  expect_identical(quiet_match("Extinct", exclude_extinct = FALSE)$worms_match, "exact")
})

test_that("saturated exact responses use fuzzy matching", {
  local_mocked_bindings(.wm_tidy_api = function(method, value) {
    lapply(value, function(name) {
      if (method == "exact") example_record(name)[rep(1L, 50), ] else example_record(name)
    })
  }, .package = "worrms2")
  expect_identical(quiet_match("Saturated")$worms_match, "fuzzy_near_1")
})

test_that("accepted records and failures are cached per call", {
  requests <- 0L
  fail <- FALSE
  local_mocked_bindings(.wm_tidy_api = function(method, value) {
    if (method == "record") {
      requests <<- requests + 1L
      if (fail) stop("Synthetic service failure")
      return(example_record("Accepted", 99))
    }
    lapply(value, function(name) example_record(name, valid_id = 99))
  }, .package = "worrms2")
  result <- quiet_match(c("Synonym1", "Synonym2"))
  expect_identical(result$valid_taxa, rep("Accepted", 2))
  expect_equal(requests, 1L)
  fail <- TRUE
  expect_warning(failed <- quiet_match(c("Synonym1", "Synonym2")), "Accepted-record lookup failed")
  expect_true(all(is.na(failed$valid_taxa)))
  expect_identical(failed$flag, c(1L, 1L))
  expect_equal(requests, 2L)
})

test_that("missing valid identifiers fall back to the parent record", {
  local_mocked_bindings(.wm_tidy_api = function(method, value) {
    if (method == "record") return(example_record("Parent", 99, rank = "Genus"))
    lapply(value, function(name) {
      record <- example_record(name, valid_id = NA)
      record$parentNameUsageID <- 99
      record
    })
  }, .package = "worrms2")
  result <- quiet_match("Child")
  expect_identical(result$valid_aphia_id, "99")
  expect_identical(result$valid_rank, "Genus")
  expect_true(is.na(result$valid_species))
  expect_identical(result$flag, 1L)
})

test_that("batch errors and alignment errors recover through single requests", {
  for (bad_shape in c(FALSE, TRUE)) {
    calls <- 0L
    local_mocked_bindings(.wm_tidy_api = function(method, value) {
      calls <<- calls + 1L
      if (length(value) > 1L) {
        if (bad_shape) return(list(NULL))
        stop("Synthetic batch failure")
      }
      lapply(value, example_record)
    }, .package = "worrms2")
    expect_warning(result <- quiet_match(c("One", "Two")), "request failed")
    expect_identical(result$flag, c(0L, 0L))
    expect_equal(calls, 3L)
  }
})

test_that("progress reports batches, original positions, elapsed time and review", {
  local_mocked_bindings(.wm_tidy_api = function(method, value) {
    lapply(value, function(name) {
      if (method == "exact" && name != "Exact") return(NULL)
      example_record(name)
    })
  }, .package = "worrms2")
  progress <- character()
  withCallingHandlers(
    invisible(wm_match(c("Typo1", "Exact", "Typo2", "Typo1 sp."), batch_size = 2)),
    message = function(m) {
      progress <<- c(progress, sub("\n$", "", conditionMessage(m)))
      invokeRestart("muffleMessage")
    }
  )
  timing <- grep("^Time difference of [0-9]+\\.[0-9]{2} secs$", progress)
  expect_length(timing, 1)
  expect_identical(progress[-timing], c(
    "Direct matching with WoRMS ...", "Batch 1/2", "Batch 2/2", "",
    "Fuzzy matching with WoRMS ...", "Typo1 1/4", "Typo2 3/4", "Typo1 sp. 4/4",
    "", "DONE", "Please double-check results"
  ))
})
