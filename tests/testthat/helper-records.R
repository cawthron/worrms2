# Synthetic API records keep checks reproducible and independent of WoRMS.
example_record <- function(name, id = 1, valid_id = id, status = "accepted",
                           marine = 1, extinct = 0, rank = "Species") {
  data.frame(scientificname = name, AphiaID = id, valid_AphiaID = valid_id,
             parentNameUsageID = NA_real_, valid_name = name, status = status,
             isMarine = marine, isExtinct = extinct, rank = rank,
             phylum = "Examplephylum", class = "Exampleclass",
             order = "Exampleorder", family = "Examplefamily",
             genus = "Example", match_type = "near_1", stringsAsFactors = FALSE)
}

quiet_match <- function(...) suppressMessages(wm_match(...))

example_validation <- function() {
  data.frame(input_taxa = "Example taxon", valid_taxa = "Accepted taxon",
             valid_phylum = "Examplephylum", valid_class = "Exampleclass",
             valid_order = "Exampleorder", valid_family = "Examplefamily",
             valid_genus = "Accepted", valid_species = "Accepted taxon",
             valid_rank = "Species", valid_aphia_id = "99", stringsAsFactors = FALSE)
}
