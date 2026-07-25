# Database helpers for 2026 MLS Value Index (SQLite)

db_connect <- function(cfg) {
  ensure_packages("DBI")
  backend <- cfg$database$backend %||% "sqlite"
  if (backend == "sqlite") {
    ensure_packages("RSQLite")
    db_path <- cfg$paths$database
    dir_create_safe(dirname(db_path))
    con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
    DBI::dbExecute(con, "PRAGMA foreign_keys = ON")
    return(con)
  }
  stop("Only sqlite backend implemented. Set database.backend: sqlite")
}

db_apply_schema <- function(con, schema_path = file.path(PROJECT_ROOT, "database", "schema_value_index.sql")) {
  if (!file.exists(schema_path)) {
    schema_path <- file.path(PROJECT_ROOT, "database", "schema.sql")
  }
  db_path <- tryCatch(con@dbname, error = function(e) NULL)
  if (!is.null(db_path) && nzchar(Sys.which("sqlite3"))) {
    status <- system2(
      "sqlite3",
      args = db_path,
      stdout = TRUE,
      stderr = TRUE,
      input = paste(readLines(schema_path, warn = FALSE), collapse = "\n")
    )
    if (length(attr(status, "status")) && attr(status, "status") != 0) {
      write_log("sqlite3 schema apply messages: ", paste(status, collapse = " | "))
    }
    return(invisible(TRUE))
  }

  raw <- readLines(schema_path, warn = FALSE)
  cleaned <- gsub("--.*$", "", raw)
  sql <- paste(cleaned, collapse = "\n")
  statements <- strsplit(sql, ";", fixed = TRUE)[[1]]
  for (stmt in statements) {
    s <- trimws(stmt)
    if (!nzchar(s)) next
    tryCatch(
      DBI::dbExecute(con, s),
      error = function(e) write_log("Schema statement warning: ", e$message)
    )
  }
  invisible(TRUE)
}

seed_reference_tables <- function(con, cfg) {
  ensure_packages("tibble")
  leagues <- tibble::tibble(
    league_id = "mls",
    league_name = "Major League Soccer",
    federation = "MLS",
    tier_code = "mls",
    country = "USA/CAN",
    is_mls_club = 1L
  )
  # optional table from legacy schema
  tryCatch(DBI::dbWriteTable(con, "leagues", leagues, overwrite = TRUE), error = function(e) invisible(NULL))

  seasons <- tibble::tibble(
    season_year = cfg$acquisition$asa$seasons %||% c(2024, 2025, 2026),
    label = as.character(cfg$acquisition$asa$seasons %||% c(2024, 2025, 2026))
  )
  DBI::dbWriteTable(con, "seasons", unique(seasons), overwrite = TRUE)

  DBI::dbWriteTable(
    con,
    "data_sources",
    data.frame(
      source_id = c("asa", "mlspa"),
      source_name = c("American Soccer Analysis", "MLSPA guaranteed compensation via ASA"),
      cutoff_date = NA_character_,
      stringsAsFactors = FALSE
    ),
    overwrite = TRUE
  )
  invisible(TRUE)
}

load_player_season_to_db <- function(con, players, teams) {
  ensure_packages("dplyr")
  if (nrow(teams)) {
    DBI::dbWriteTable(
      con,
      "teams",
      teams |>
        dplyr::select(dplyr::any_of(c(
          "team_id", "team_name", "team_short", "league_id", "is_mls_club", "asa_team_id"
        ))),
      overwrite = TRUE
    )
  }

  player_dim <- players |>
    dplyr::distinct(asa_player_id, .keep_all = TRUE) |>
    dplyr::transmute(
      player_id = paste0("asa_", asa_player_id),
      asa_player_id = as.character(asa_player_id),
      display_name = display_name,
      normalized_name = dplyr::coalesce(normalized_name, display_name),
      birth_date = if ("birth_date" %in% names(players)) as.character(birth_date) else NA_character_,
      nationality = if ("nationality" %in% names(players)) as.character(nationality) else NA_character_,
      primary_position = if ("primary_position" %in% names(players)) as.character(primary_position) else NA_character_
    )
  DBI::dbWriteTable(con, "players", player_dim, overwrite = TRUE)

  stats <- players |>
    dplyr::filter(league_id == "mls") |>
    dplyr::transmute(
      asa_player_id = as.character(asa_player_id),
      season_year = as.integer(season_year),
      team_id = as.character(team_id),
      minutes = as.numeric(minutes),
      goals_added_p90 = as.numeric(goals_added_p90)
    )
  DBI::dbWriteTable(con, "player_season_stats", stats, overwrite = TRUE)

  if (all(c(
    "goals_added_shooting_p90", "goals_added_passing_p90", "goals_added_receiving_p90",
    "goals_added_dribbling_p90", "goals_added_defending_p90", "goals_added_fouling_p90"
  ) %in% names(players))) {
    gplus <- players |>
      dplyr::filter(league_id == "mls") |>
      dplyr::group_by(asa_player_id, season_year) |>
      dplyr::summarise(
        shooting_p90 = stats::weighted.mean(goals_added_shooting_p90, w = pmax(minutes, 1), na.rm = TRUE),
        passing_p90 = stats::weighted.mean(goals_added_passing_p90, w = pmax(minutes, 1), na.rm = TRUE),
        receiving_p90 = stats::weighted.mean(goals_added_receiving_p90, w = pmax(minutes, 1), na.rm = TRUE),
        dribbling_p90 = stats::weighted.mean(goals_added_dribbling_p90, w = pmax(minutes, 1), na.rm = TRUE),
        interrupting_p90 = stats::weighted.mean(goals_added_defending_p90, w = pmax(minutes, 1), na.rm = TRUE),
        fouling_p90 = stats::weighted.mean(goals_added_fouling_p90, w = pmax(minutes, 1), na.rm = TRUE),
        .groups = "drop"
      )
    DBI::dbWriteTable(con, "goals_added_components", gplus, overwrite = TRUE)
  }

  if ("salary" %in% names(players)) {
    comp <- players |>
      dplyr::filter(league_id == "mls", is.finite(salary), salary > 0) |>
      dplyr::group_by(asa_player_id, season_year) |>
      dplyr::summarise(
        guaranteed_compensation = max(salary, na.rm = TRUE),
        source = "MLSPA via ASA",
        as_of_date = NA_character_,
        .groups = "drop"
      )
    DBI::dbWriteTable(con, "compensation_records", comp, overwrite = TRUE)
  }
  invisible(TRUE)
}

load_value_scores_to_db <- function(con, scores_path) {
  if (!file.exists(scores_path)) return(invisible(FALSE))
  ensure_packages("readr")
  scores <- readr::read_csv(scores_path, show_col_types = FALSE)
  out <- scores |>
    dplyr::transmute(
      asa_player_id = as.character(asa_player_id),
      evaluation_period = as.character(evaluation_period),
      position_group = as.character(position_group),
      sporting_impact = as.numeric(sporting_impact),
      compensation_percentile = as.numeric(compensation_percentile),
      value_surplus = as.numeric(value_surplus),
      undervaluation_score = as.numeric(undervaluation_score),
      metric_coverage = as.numeric(metric_coverage),
      data_confidence = as.character(data_confidence),
      model_version = as.character(model_version),
      data_version = as.character(data_version %||% data_cutoff_label),
      calculation_timestamp = as.character(calculation_timestamp)
    )
  DBI::dbWriteTable(con, "player_value_scores", out, overwrite = TRUE)
  invisible(TRUE)
}

`%||%` <- function(a, b) if (!is.null(a)) a else b
