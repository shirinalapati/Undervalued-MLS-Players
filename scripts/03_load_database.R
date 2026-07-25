#!/usr/bin/env Rscript
# 03_load_database.R — load MLS Value Index tables into SQLite

source("R/utilities/load_project.R")
source("R/database/db_utils.R")

cfg <- load_config()
ensure_packages(c("readr", "dplyr", "DBI", "RSQLite"))

db_path <- cfg$paths$database
if (file.exists(db_path)) {
  write_log("Removing existing DB for clean reload: ", db_path)
  file.remove(db_path)
}

con <- db_connect(cfg)
on.exit(DBI::dbDisconnect(con), add = TRUE)

db_apply_schema(con)
seed_reference_tables(con, cfg)

multi_path <- file.path(cfg$paths$interim, "player_season_multi.csv")
teams_path <- file.path(cfg$paths$interim, "teams_clean.csv")
if (!file.exists(multi_path)) stop("Missing interim player_season_multi.csv")

players <- readr::read_csv(multi_path, show_col_types = FALSE)
teams <- if (file.exists(teams_path)) readr::read_csv(teams_path, show_col_types = FALSE) else dplyr::tibble()
load_player_season_to_db(con, players, teams)

# Prefer value scores if already built; otherwise leave empty for later step
for (period in c("blended", "ytd_2026", "full_2025")) {
  path <- file.path(cfg$paths$processed, paste0("player_value_scores_", period, ".csv"))
  if (file.exists(path)) {
    load_value_scores_to_db(con, path)
    write_log("Loaded value scores from ", path)
    break
  }
}

DBI::dbExecute(
  con,
  "INSERT INTO pipeline_runs (started_at, finished_at, model_version, status) VALUES (?, ?, ?, ?)",
  params = list(
    as.character(Sys.time()), as.character(Sys.time()),
    "1.0.0", "ok"
  )
)

write_log("Database ready: ", db_path)
