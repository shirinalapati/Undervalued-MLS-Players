#!/usr/bin/env Rscript
# Install dependencies (use renv in real workflow)

pkgs <- c(
  "DBI", "RSQLite", "dplyr", "ggplot2", "here", "jsonlite", "openxlsx",
  "purrr", "readr", "rmarkdown", "shiny", "DT", "tibble", "tidyr", "yaml",
  "markdown", "testthat", "itscalledsoccer"
)

inst <- rownames(installed.packages())
need <- setdiff(pkgs, inst)
if (length(need)) {
  install.packages(need, repos = "https://cloud.r-project.org")
}

# Optional live ASA client
if (!requireNamespace("itscalledsoccer", quietly = TRUE)) {
  message("Installing itscalledsoccer for live ASA mode...")
  install.packages("itscalledsoccer", repos = "https://cloud.r-project.org")
}

message("Dependencies ready.")
