# Daily ASA/MLSPA refresh + Value Index rebuild.
# Cron (America/Los_Angeles):
#   15 6 * * * cd /path/to/mls-recruitment-intelligence && Rscript scripts/refresh_daily.R >> logs/refresh.log 2>&1
#
# Force refresh:
#   MLS_RI_FORCE_REFRESH=1 Rscript scripts/run_pipeline.R --force-refresh --skip-reports

args <- commandArgs(trailingOnly = TRUE)
root <- normalizePath(getwd())
if (basename(root) == "scripts") root <- dirname(root)
setwd(root)

dir.create("logs", showWarnings = FALSE)

Sys.setenv(MLS_RI_FORCE_REFRESH = "1")
status <- system2(
  "Rscript",
  c("scripts/run_pipeline.R", "--force-refresh", "--skip-reports", args)
)
if (!identical(status, 0L)) {
  quit(status = status)
}

source("R/utilities/load_project.R")
source("R/utilities/data_provenance.R")
cfg <- load_config()
prov <- read_provenance(cfg)
message("Daily refresh complete. ", cutoff_label(prov, cfg = cfg))
message(
  "Shiny apps with auto_reload will pick up new processed files within ~",
  cfg$deployment$refresh_check_seconds %||% 60, " seconds."
)
