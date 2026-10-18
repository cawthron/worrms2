# worrms2

Validate taxonomic names against the **World Register of Marine Species
(WoRMS)**, review uncertain matches, and add accepted taxonomy to ecological
observations.

It builds on the official [`worrms`](https://docs.ropensci.org/worrms/) client.


`worrms2` provides three public functions:

| Function | Purpose |
|---|---|
| `wm_match()` | Clean names, perform exact and fuzzy searches, retrieve accepted taxonomy, and flag results for review. |
| `add_wm_taxonomy()` | Join a reviewed validation table to an observation data frame. |
| `worrms2_method()` | View a template for describing this workflow in a methods section. |

More info is available via:


```r
?wm_match()
?add_wm_taxonomy()
?worrms2_method()
```



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

The package requires **R >= 4.1.0**. Its eight direct runtime dependencies are
`dplyr` (>= 1.1.1), `httr`, `jsonlite`, `mgsub`, `stringi`, `stringr`, `tibble`,
and `worrms` (>= 0.4.3). These and their dependencies are installed automatically.
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



## References and citation

* [World Register of Marine Species](https://www.marinespecies.org/)
* [WoRMS REST API](https://www.marinespecies.org/rest/)
* [`worrms` client documentation](https://docs.ropensci.org/worrms/)

Use `citation("worrms2")` for the software citation. Also cite WoRMS and record
the access date according to the database's current citation guidance. Package
licensing and database terms are separate.
