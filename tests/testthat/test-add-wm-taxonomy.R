test_that("joining retains observations, unmatched names, and factor levels", {
  observations <- data.frame(label = c(" Example taxon ", "Unmatched", "Example taxon"),
                              group = factor(c("b", "a", "b"), levels = c("b", "a")),
                              phylum = "obsolete")
  result <- add_wm_taxonomy(observations, example_validation(), by_taxa = "label")
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 3)
  expect_identical(result$original_taxa, trimws(observations$label))
  expect_identical(result$taxa, c("Accepted taxon", "Unmatched", "Accepted taxon"))
  expect_identical(result$group, observations$group)
  expect_identical(names(result)[1:9], c("phylum", "class", "order", "family", "genus",
                                       "species", "rank", "aphia_id", "taxa"))
  expect_identical(result$phylum, c("Examplephylum", NA_character_, "Examplephylum"))
})

test_that("the standalone helper's validation-key whitespace behaviour is retained", {
  validation <- example_validation()
  validation$input_taxa <- " Example taxon "
  result <- add_wm_taxonomy(data.frame(taxa = " Example taxon "), validation)
  expect_identical(result$original_taxa, "Example taxon")
  expect_identical(result$taxa, "Example taxon")
  expect_true(is.na(result$aphia_id))
})

test_that("duplicate validation keys retain the original join semantics", {
  validation <- example_validation()[c(1, 1), ]
  result <- add_wm_taxonomy(data.frame(taxa = "Example taxon"), validation)
  expect_equal(nrow(result), 2)
})

test_that("the join handles empty inputs and reports invalid inputs", {
  validation <- example_validation()
  expect_equal(nrow(add_wm_taxonomy(data.frame(taxa = character()), validation)), 0)
  expect_error(add_wm_taxonomy(data.frame(taxa = "Example"), data.frame()), "wm_match")
  expect_error(add_wm_taxonomy(data.frame(taxa = "Example"), validation, "absent"), "by_taxa")
})

test_that("both CSV examples are installed with the package", {
  for (file in c("example_data.csv", "example_data2.csv")) {
    path <- system.file("extdata", file, package = "worrms2", mustWork = TRUE)
    expect_true(file.exists(path))
  }
})
