# Database helpers (SQLite MVP)

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
  stop("Only sqlite backend implemented in MVP. Set database.backend: sqlite")
}

db_apply_schema <- function(con, schema_path = file.path(PROJECT_ROOT, "database", "schema.sql")) {
  db_path <- tryCatch(con@dbname, error = function(e) NULL)
  if (!is.null(db_path) && nzchar(Sys.which("sqlite3"))) {
    # Feed schema via stdin to avoid .read path quoting issues
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
  ensure_packages(c("dplyr", "tibble"))
  roles_yaml <- load_yaml("config/role_weights.yml")
  tiers <- load_yaml("config/league_tiers.yml")
  clubs <- load_yaml("config/club_profiles.yml")

  leagues <- tibble::tibble(
    league_id = c("mls", "mlsnp", "uslc", "usl1", "cpl", "ncaa"),
    league_name = c("Major League Soccer", "MLS NEXT Pro", "USL Championship",
                    "USL League One", "Canadian Premier League", "NCAA"),
    federation = c("MLS", "MLS", "USL", "USL", "CPL", "NCAA"),
    tier_code = c("mls", "mlsnp", "uslc", "usl1", "cpl", "ncaa"),
    country = c("USA/CAN", "USA", "USA", "USA", "CAN", "USA"),
    is_mls_recruitment_market = c(1L, 1L, 1L, 1L, 1L, 1L)
  )
  DBI::dbWriteTable(con, "leagues", leagues, append = TRUE)

  seasons <- tibble::tibble(
    season_year = cfg$acquisition$asa$seasons %||% cfg$project$product_season %||% 2026,
    label = as.character(cfg$acquisition$asa$seasons %||% cfg$project$product_season %||% 2026)
  )
  seasons <- unique(seasons)
  DBI::dbWriteTable(con, "seasons", seasons, append = TRUE)

  role_rows <- purrr::imap_dfr(roles_yaml$roles, function(role, id) {
    tibble::tibble(
      role_id = id,
      display_name = role$display_name,
      position_group = role$position_group,
      description = role$description,
      is_mvp = 1L
    )
  })
  DBI::dbWriteTable(con, "tactical_roles", role_rows, append = TRUE)

  weight_rows <- purrr::imap_dfr(roles_yaml$roles, function(role, id) {
    tibble::tibble(
      role_id = id,
      metric_name = names(role$metrics),
      weight = as.numeric(unlist(role$metrics))
    )
  })
  DBI::dbWriteTable(con, "role_metric_weights", weight_rows, append = TRUE)

  # Translation factors
  tf <- purrr::imap_dfr(tiers$tiers, function(t, id) {
    tibble::tibble(
      league_id = id,
      metric_family = c("attack", "creation", "defense"),
      translation_factor = c(t$translation_factor_attack, t$translation_factor_creation, t$translation_factor_defense),
      uncertainty = t$uncertainty,
      model_version = "mvp_tier_v1",
      notes = t$notes
    )
  })
  # Only write leagues that exist in leagues table
  tf <- dplyr::filter(tf, league_id %in% leagues$league_id)
  DBI::dbWriteTable(con, "league_translation_factors", tf, append = TRUE)

  club_df <- dplyr::bind_rows(lapply(clubs$clubs, as.data.frame, stringsAsFactors = FALSE))
  club_df$profile_label <- clubs$meta$label
  club_df$season_year <- clubs$meta$season
  # ensure column order-ish
  DBI::dbWriteTable(con, "club_profiles", club_df, append = TRUE)

  if (!is.null(clubs$example_needs)) {
    needs <- dplyr::bind_rows(lapply(clubs$example_needs, as.data.frame, stringsAsFactors = FALSE))
    DBI::dbWriteTable(con, "club_needs", needs, append = TRUE)
  }

  invisible(TRUE)
}

load_player_season_to_db <- function(con, players_df, teams_df) {
  ensure_packages("dplyr")

  player_dim <- players_df |>
    dplyr::distinct(player_id, display_name, normalized_name, is_domestic_player, primary_position = position_group) |>
    dplyr::mutate(
      birth_date = NA_character_,
      nationality = NA_character_,
      asa_player_id = NA_character_,
      fbref_player_id = NA_character_,
      height_cm = NA_real_,
      preferred_foot = NA_character_
    )

  DBI::dbWriteTable(con, "players", as.data.frame(player_dim), append = TRUE)
  DBI::dbWriteTable(con, "teams", as.data.frame(teams_df), append = TRUE)

  pos <- players_df |>
    dplyr::transmute(player_id, season_year, position_group, position_detail = tactical_role_primary, is_primary = 1L)
  DBI::dbWriteTable(con, "player_positions", as.data.frame(pos), append = TRUE)

  pss <- players_df |>
    dplyr::transmute(
      player_id, team_id, league_id, season_year, minutes,
      games = NA_integer_,
      npxg = npxg_p90 * minutes / 90,
      xa = xa_p90 * minutes / 90,
      shots = shots_p90 * minutes / 90,
      goals = NA_real_,
      assists = NA_real_,
      xpass_diff = NA_real_,
      goals_added = goals_added_p90 * minutes / 90,
      goals_added_dribbling = NA_real_,
      goals_added_passing = NA_real_,
      goals_added_receiving = NA_real_,
      goals_added_shooting = NA_real_,
      goals_added_defending = NA_real_,
      tackles = tackles_p90 * minutes / 90,
      interceptions = interceptions_p90 * minutes / 90,
      pressures_proxy = pressures_p90 * minutes / 90,
      progressive_passes_proxy = progressive_passes_p90 * minutes / 90,
      progressive_carries_proxy = progressive_carries_p90 * minutes / 90,
      aerial_wins = NA_real_,
      aerial_duels = NA_real_,
      raw_payload_json = NA_character_,
      source = data_source,
      retrieved_at = as.character(Sys.time())
    )
  DBI::dbWriteTable(con, "player_season_stats", as.data.frame(pss), append = TRUE)

  sal <- players_df |>
    dplyr::filter(!is.na(salary)) |>
    dplyr::transmute(
      player_id, team_id, season_year,
      base_salary = salary * 0.9,
      guaranteed_comp = salary,
      currency = "USD",
      as_of_date = paste0(season_year, "-07-01"),
      source = data_source,
      retrieved_at = as.character(Sys.time())
    )
  if (nrow(sal)) DBI::dbWriteTable(con, "salary_records", as.data.frame(sal), append = TRUE)

  invisible(TRUE)
}

`%||%` <- function(a, b) if (!is.null(a)) a else b
