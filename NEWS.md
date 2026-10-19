# worrms2 0.1.0

* Restore the original step-by-step `wm_match()` implementation.
* Align help, README, and offline tests with individual fuzzy and accepted-record
  requests, numeric review flags 0/1/2, and the original error handling.
* Import the functions used by the original pipelines explicitly; remove the
  unused direct `tibble` dependency (it remains an indirect dependency).
* Package the existing dplyr-based WoRMS matcher and standalone taxonomy join.
* Export `wm_match()` and `add_wm_taxonomy()` with generated R help pages.
* Declare runtime imports and separate the development environment from installation.
* Install the two example CSVs under `extdata` and add offline package tests.
* Retain batch progress, fuzzy taxon progress, elapsed time, and the review reminder.
