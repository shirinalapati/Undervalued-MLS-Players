#!/usr/bin/env Rscript
# Install dependencies (use renv in real workflow)

pkgs <- c(
  "DBI", "RSQLite", "dplyr", "ggplot2", "here", "jsonlite", "openxlsx",
  "purrr", "readr", "rmarkdown", "shiny", "DT", "tibble", "tidyr", "yaml",
  "markdown", "testthat", "itscalledsoccer"
)

`%||%` <- function(a, b) if (!is.null(a) && length(a) && nzchar(as.character(a)[[1]])) a else b

# Prefer whatever repos the environment configured (RSPM binaries on GitHub Actions).
repos <- getOption("repos")
cran <- tryCatch(unname(repos[["CRAN"]]), error = function(e) NULL)
if (is.null(repos) || identical(cran, "@CRAN@") || !nzchar(cran %||% "")) {
  repos <- c(CRAN = "https://cloud.r-project.org")
}

inst <- rownames(installed.packages())
need <- setdiff(pkgs, inst)
if (length(need)) {
  install.packages(need, repos = repos)
}

# Required for live ASA / MLSPA pulls
if (!requireNamespace("itscalledsoccer", quietly = TRUE)) {
  message("Installing itscalledsoccer for live ASA mode...")
  install.packages("itscalledsoccer", repos = repos)
}
if (!requireNamespace("itscalledsoccer", quietly = TRUE)) {
  stop("Failed to install itscalledsoccer — live ASA refresh cannot run.")
}

message("Dependencies ready. CRAN repo: ", repos[["CRAN"]] %||% as.character(repos[[1]]))
