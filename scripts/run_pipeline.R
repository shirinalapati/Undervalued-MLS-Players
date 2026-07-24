#!/usr/bin/env Rscript
# Run the full MVP pipeline in order.
# Usage:
#   Rscript scripts/run_pipeline.R
#   Rscript scripts/run_pipeline.R --skip-reports
#   Rscript scripts/run_pipeline.R --force-refresh --skip-reports
#   Rscript scripts/refresh_daily.R

args <- commandArgs(trailingOnly = TRUE)
force_demo <- "--demo" %in% args
skip_reports <- "--skip-reports" %in% args
force_refresh <- "--force-refresh" %in% args

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
write_log("Pipeline mode: ", cfg$project$mode, " | product season: ", cfg$project$product_season %||% 2026)

steps <- c(
  if (identical(cfg$project$mode, "demo")) "scripts/00_generate_demo_data.R" else NULL,
  "scripts/01_collect_data.R",
  "scripts/02_clean_data.R",
  "scripts/03_load_database.R",
  "scripts/04_build_features.R",
  "scripts/05_train_models.R",
  "scripts/06_generate_rankings.R"
)
steps <- Filter(Negate(is.null), steps)
if (!skip_reports) steps <- c(steps, "scripts/07_generate_reports.R")

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
write_log("Launch dashboard with: Rscript -e \"shiny::runApp('app', port = 7788)\"")
