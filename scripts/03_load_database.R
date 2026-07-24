#!/usr/bin/env Rscript
# 03_load_database.R

source("R/utilities/load_project.R")
source("R/database/db_utils.R")

cfg <- load_config()
ensure_packages(c("readr", "dplyr", "purrr", "DBI", "RSQLite"))

# Fresh DB for reproducibility in MVP
db_path <- cfg$paths$database
if (file.exists(db_path)) {
  write_log("Removing existing DB for clean reload: ", db_path)
  file.remove(db_path)
}

con <- db_connect(cfg)
on.exit(DBI::dbDisconnect(con), add = TRUE)

db_apply_schema(con)
seed_reference_tables(con, cfg)

players <- readr::read_csv(file.path(cfg$paths$interim, "player_season_clean.csv"), show_col_types = FALSE)
teams <- readr::read_csv(file.path(cfg$paths$interim, "teams_clean.csv"), show_col_types = FALSE)
load_player_season_to_db(con, players, teams)

DBI::dbWriteTable(
  con,
  "data_sources",
  data.frame(
    source_name = ifelse(cfg$project$mode == "live", "ASA API / itscalledsoccer", "demo_synthetic"),
    source_url = ifelse(cfg$project$mode == "live",
                        "https://app.americansocceranalysis.com/api/v1/",
                        "data/external/demo"),
    entity_type = "player_season",
    league_id = NA_character_,
    season_year = cfg$project$product_season %||% 2026,
    retrieved_at = as.character(Sys.time()),
    record_count = nrow(players),
    checksum = NA_character_,
    notes = "Loaded via scripts/03_load_database.R",
    stringsAsFactors = FALSE
  ),
  append = TRUE
)

write_log("Database load complete: ", db_path)
