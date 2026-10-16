local({
  # Keep the existing project-local library when renv detects a package project.
  paths <- c(RENV_PATHS_LIBRARY = "library", RENV_PATHS_ROOT = "local",
              RENV_PATHS_SANDBOX = "sandbox")
  for (variable in names(paths)) {
    if (!nzchar(Sys.getenv(variable))) {
      do.call(Sys.setenv, stats::setNames(
        list(file.path(getwd(), "renv", paths[[variable]])), variable
      ))
    }
  }
  source("renv/activate.R")
})
