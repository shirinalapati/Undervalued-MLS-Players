#!/usr/bin/env Rscript
# 02_clean_data.R

source("R/utilities/load_project.R")
source("R/clean/clean_players.R")

cfg <- load_config()

if (identical(cfg$project$mode, "live") &&
    file.exists(file.path(cfg$paths$raw, "asa_collection.rds"))) {
  write_log("Cleaning ASA collection into multi-season standardized schema...")
  clean_asa_collection(cfg)
} else if (identical(cfg$project$mode, "live")) {
  write_log("Live mode requested but no ASA collection — falling back to synthetic demo clean.")
  clean_demo_players(cfg)
} else {
  write_log("Cleaning demo players (synthetic)...")
  clean_demo_players(cfg)
}

n <- tryCatch(
  nrow(readr::read_csv(file.path(cfg$paths$interim, "player_season_multi.csv"), show_col_types = FALSE)),
  error = function(e) NA_integer_
)
write_log("Cleaning complete. Multi-season rows: ", n)
