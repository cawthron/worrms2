# worrms2

Validate taxonomic names against the **World Register of Marine Species
(WoRMS)**, review uncertain matches, and add accepted taxonomy to ecological
observations.

It builds on the official [`worrms`](https://docs.ropensci.org/worrms/) client.


`worrms2` provides three public functions:

| Function | Purpose |
|---|---|
| `wm_match()` | Clean names, perform exact and fuzzy searches, retrieve accepted taxonomy, and flag results for review. |
| `add_wm_taxonomy()` | Join a reviewed validation table to any dataframe containing original taxa names. |
| `worrms2_method()` | View a template for describing this workflow in a methods section. |

More info is available via:


```r
?wm_match()
?add_wm_taxonomy()
?worrms2_method()
```

`wm_match()` is very similar to `worrms::wm_records_taxamatch` while being able to handle more names at once, providing some quality control, and returning outputs as data frame (instead of list).


## Installation


```r
install.packages("devtools")  # Once, if needed
devtools::install_github("cawthron/worrms2")
library(worrms2)
```

Alternatively, use the smaller installation helper:

```r
install.packages("remotes")
remotes::install_github("cawthron/worrms2")
```

The package requires **R >= 4.1.0**. Its seven direct CRAN runtime dependencies
are `dplyr` (>= 1.1.1), `httr`, `jsonlite`, `mgsub`, `stringi`, `stringr`,
and `worrms` (>= 0.4.3), plus the standard R packages `stats` and `utils`.
CRAN dependencies are installed automatically. `tibble` remains an indirect
dependency through `dplyr`.
The full `tidyverse` is not required. Loading `worrms2` does not attach its
dependencies to the search path.

If package updates cause problems try:

```r
remotes::install_github("cawthron/worrms2", upgrades= 'never')
```


If curl installation fails, try using the latest pre-compiled binary with:

```r
install.packages("curl", type = "binary")
```



## Matching and review

`wm_match()` retains the original step-by-step matching workflow. Supply a
non-empty character vector without `NA`, using names that remain non-empty
after cleaning. Exact searches run in batches of 49 by default; supply an
integer from 1 to 50 for `batch_size`. Invalid batch sizes and empty input are
unsupported.

```r
validation <- wm_match(
  c("Abra alba", "Bivalvia Unid. (juv)"),
  eval_as = c("Bivalvia Unid. (juv)" = "Bivalvia")
)
review <- validation[validation$flag > 0, ]
# Review and correct validation before joining it to observations.
```

Original duplicate names are collapsed, preserving first-occurrence order.
Different original names sharing a cleaned name still have separate lookups.
Names without an exact match, or with an exact response of 50 records, receive
individual fuzzy searches. Accepted or parent records are retrieved individually
when their identifiers differ from the matched record; responses are not cached.

Candidate preferences run in order: exclude explicitly extinct records, prefer
marine records, then prefer accepted records. Remaining ties use the first
record. Exact searches retain the `worrms` client's default marine restriction;
fuzzy searches use `marine_only = FALSE`. Setting `prio_marine = FALSE` changes
candidate preference without broadening the exact search.

The returned data frame has 17 columns, including original and cleaned names,
match method and status, matched and accepted identifiers, and classification.
The numeric `flag` column indicates:

| Flag | Meaning |
|---|---|
| 0 | No implemented review rule triggered. |
| 1 | Multiple candidates or the input differs from the accepted name. |
| 2 | No match, missing match method, changed identifier or rank, or matched status other than accepted. |

Flag 2 takes precedence over flag 1. Review both flagged results and missing
classification: flag 0 does not guarantee complete or correct taxonomy.
Failed exact batches stop the function. Errors during individual lookups are
generally suppressed; if an accepted-record lookup fails, classification can
still come from the matched record. The function reports batch and fuzzy-name
progress, prints elapsed time, and reminds you to check results.

## Development environment

Open this project in RStudio or Positron. The project `.Rprofile` activates an
isolated development library in `renv/library/`. On a fresh clone, restore the
recorded versions before developing:

```r
renv::restore()
renv::status()
```

To intentionally update development dependencies to the latest available CRAN
versions, run the following, then restart R and run the checks below:

```r
renv::update()
renv::snapshot()
renv::status()
```

The explicit snapshot includes the dependencies in `DESCRIPTION`, including
`Suggests`, and their required dependencies. Commit `renv.lock`, `.Rprofile`,
`renv/activate.R`, and `renv/settings.json` after reviewing dependency changes.
The installed library and cache stay local and are ignored by Git; collaborators
recreate the library with `renv::restore()`. Restore uses recorded versions;
updates are intentional, rather than automatic at startup.

Normal users can install with `remotes::install_github()` or
`devtools::install_github()` as shown above, without activating or restoring renv.
Runtime dependencies come from `DESCRIPTION`; renv and the other development
tools are optional `Suggests`. `.Rbuildignore` excludes the renv library,
lockfile, and startup configuration from the built package.

Regenerate help and imports with `roxygen2::roxygenise()`, run offline tests with
`devtools::test()`, and check the package with `devtools::check()`.

## References and citation

* [World Register of Marine Species](https://www.marinespecies.org/)
* [WoRMS REST API](https://www.marinespecies.org/rest/)
* [`worrms` client documentation](https://docs.ropensci.org/worrms/)

Use `citation("worrms2")` for the software citation. Also cite WoRMS and record
the access date according to the database's current citation guidance. Package
licensing and database terms are separate.
