#!/usr/bin/env Rscript
# 01_collect_data.R
# Optional: --force-refresh  or  MLS_RI_FORCE_REFRESH=1  to ignore ASA cache TTL

source("R/utilities/load_project.R")
source("R/collect/demo_generate.R")
source("R/collect/asa_collect.R")

args <- commandArgs(trailingOnly = TRUE)
force_refresh <- "--force-refresh" %in% args || identical(Sys.getenv("MLS_RI_FORCE_REFRESH"), "1")

cfg <- load_config()
dir_create_safe(cfg$paths$raw)

if (identical(cfg$project$mode, "live")) {
  write_log(
    "Live mode: collecting ASA 2024/2025/2026 (+ 2026 MLS salary guide)",
    if (force_refresh) " [FORCE REFRESH]" else paste0(" [cache TTL ", cfg$acquisition$asa$cache_hours %||% 12, "h]")
  )
  tryCatch(
    collect_live_asa(cfg, force = force_refresh),
    error = function(e) {
      write_log("Live collection failed (", e$message, ").")
      write_log("Keeping any existing ASA cache; demo fallback will be used only if clean finds no ASA collection.")
      if (!file.exists(file.path(cfg$paths$demo, "demo_player_season.csv"))) {
        generate_demo_cohort(cfg)
      }
    }
  )
} else {
  write_log("Demo mode: ensuring synthetic cohort exists.")
  if (!file.exists(file.path(cfg$paths$demo, "demo_player_season.csv"))) {
    generate_demo_cohort(cfg)
  } else {
    write_log("Demo cohort already present.")
  }
}

write_log("Collection step complete.")
