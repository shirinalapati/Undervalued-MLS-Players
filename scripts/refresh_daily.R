#!/usr/bin/env Rscript
# Daily / scheduled refresh for deployed live-season product.
# Re-pulls ASA + MLSPA (force), rebuilds features/scores/rankings.
# The Shiny app hot-reloads processed files — no app restart required.
#
# Cron example (06:15 America/Los_Angeles daily):
#   15 6 * * * cd /path/to/mls-recruitment-intelligence && Rscript scripts/refresh_daily.R >> logs/refresh.log 2>&1
#
# Or: MLS_RI_FORCE_REFRESH=1 Rscript scripts/run_pipeline.R --skip-reports --force-refresh

args <- commandArgs(trailingOnly = TRUE)
root <- normalizePath(getwd())
if (basename(root) == "scripts") root <- dirname(root)
setwd(root)

dir.create("logs", showWarnings = FALSE)

Sys.setenv(MLS_RI_FORCE_REFRESH = "1")
status <- system2(
  "Rscript",
  c("scripts/run_pipeline.R", "--skip-reports", "--force-refresh", args)
)
if (!identical(status, 0L)) {
  quit(status = status)
}

source("R/utilities/load_project.R")
source("R/utilities/data_provenance.R")
cfg <- load_config()
prov <- read_provenance(cfg)
message("Daily refresh complete. ", cutoff_label(prov))
