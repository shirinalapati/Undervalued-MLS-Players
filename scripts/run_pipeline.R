#!/usr/bin/env Rscript
# Run the 2026 MLS Value Index pipeline.
# Usage:
#   Rscript scripts/run_pipeline.R
#   Rscript scripts/run_pipeline.R --skip-reports
#   Rscript scripts/run_pipeline.R --force-refresh --skip-reports

args <- commandArgs(trailingOnly = TRUE)
force_demo <- "--demo" %in% args
skip_reports <- "--skip-reports" %in% args
force_refresh <- "--force-refresh" %in% args
skip_collect <- "--skip-collect" %in% args

root <- normalizePath(getwd())
if (basename(root) == "scripts") root <- dirname(root)
setwd(root)

source("R/utilities/load_project.R")
cfg_path <- file.path(PROJECT_ROOT, "config", "config.yml")

if (force_demo) {
  write_log("Forcing synthetic demo mode for this run...")
  raw <- readLines(cfg_path)
  raw <- sub('mode: "live"', 'mode: "demo"', raw, fixed = TRUE)
  writeLines(raw, cfg_path)
}

if (force_refresh) {
  Sys.setenv(MLS_RI_FORCE_REFRESH = "1")
  write_log("Force-refresh enabled: ASA/MLSPA caches will be re-fetched.")
}

cfg <- load_config()
write_log("Pipeline mode: ", cfg$project$mode, " | product: ", cfg$project$name)

steps <- c(
  if (identical(cfg$project$mode, "demo")) "scripts/00_generate_demo_data.R" else NULL,
  if (!skip_collect) "scripts/01_collect_data.R" else NULL,
  if (!skip_collect) "scripts/02_clean_data.R" else NULL,
  "scripts/06_generate_value_index.R",
  "scripts/03_load_database.R",
  "scripts/07_generate_reports.R",
  "scripts/09_run_validation.R",
  "scripts/10_run_tests.R"
)
if (skip_reports) {
  steps <- setdiff(steps, c("scripts/07_generate_reports.R"))
}
steps <- Filter(Negate(is.null), steps)
# Drop steps that do not exist yet
steps <- steps[file.exists(steps)]

for (step in steps) {
  write_log("==== ", step, " ====")
  step_args <- character()
  if (identical(step, "scripts/01_collect_data.R") && force_refresh) {
    step_args <- "--force-refresh"
  }
  status <- system2("Rscript", c(step, step_args))
  if (!identical(status, 0L)) {
    stop("Pipeline failed at ", step, " (exit ", status, ")")
  }
}

if (force_demo) {
  raw <- readLines(cfg_path)
  raw <- sub('mode: "demo"', 'mode: "live"', raw, fixed = TRUE)
  writeLines(raw, cfg_path)
  write_log("Restored config project.mode to live after forced demo run.")
}

write_log("Pipeline complete.")
write_log("Launch dashboard: Rscript -e \"shiny::runApp('app', port = 7788)\"")
