# Clean / standardize player-season tables into analysis-ready interim files.

clean_demo_players <- function(cfg) {
  ensure_packages(c("dplyr", "readr"))
  source(file.path(PROJECT_ROOT, "R/utilities/data_provenance.R"), local = TRUE)

  raw <- readr::read_csv(file.path(cfg$paths$demo, "demo_player_season.csv"), show_col_types = FALSE)
  teams <- readr::read_csv(file.path(cfg$paths$demo, "demo_teams.csv"), show_col_types = FALSE)

  players <- raw |>
    dplyr::transmute(
      player_id,
      display_name,
      normalized_name,
      birth_date = NA_character_,
      nationality = dplyr::if_else(is_domestic_player == 1L, "USA/CAN", "INT"),
      primary_position = position_group,
      is_domestic_player,
      intl_roster_status = if ("intl_roster_status" %in% names(raw)) intl_roster_status else NA_character_,
      nationality_hint_usa_can = is_domestic_player,
      asa_player_id = player_id,
      age,
      league_id,
      team_id,
      season_year = as.integer(cfg$project$product_season %||% 2026),
      season_status = "synthetic",
      position_group,
      tactical_role_primary = tactical_role,
      minutes,
      npxg_p90,
      xa_p90,
      shots_p90,
      pressures_p90,
      tackles_p90,
      interceptions_p90,
      progressive_passes_p90,
      progressive_carries_p90,
      goals_added_p90,
      aerial_win_pct,
      pass_completion_pct,
      crosses_p90,
      yoy_delta,
      salary,
      cost_tier,
      compensation_known = if ("compensation_known" %in% names(raw)) compensation_known else TRUE,
      guaranteed_compensation_tier = cost_tier,
      financial_data_confidence = if ("financial_data_confidence" %in% names(raw)) financial_data_confidence else "demo_synthetic_compensation",
      minutes_share,
      data_source = "demo"
    )

  dir_create_safe(cfg$paths$interim)
  readr::write_csv(players, file.path(cfg$paths$interim, "player_season_multi.csv"))
  readr::write_csv(players, file.path(cfg$paths$interim, "player_season_clean.csv"))
  readr::write_csv(teams, file.path(cfg$paths$interim, "teams_clean.csv"))

  prov <- make_demo_provenance(cfg)
  prov$n_players <- dplyr::n_distinct(players$player_id)
  write_provenance(cfg, prov)
  write_log("Wrote SYNTHETIC demo interim + provenance.")
  invisible(players)
}

# Live ASA flattening — multi-season standardized schema + provenance.
clean_asa_collection <- function(cfg) {
  path <- file.path(cfg$paths$raw, "asa_collection.rds")
  if (!file.exists(path)) {
    write_log("No ASA collection found; falling back to demo clean.")
    return(clean_demo_players(cfg))
  }

  ensure_packages(c("dplyr", "tidyr", "purrr", "readr"))
  source(file.path(PROJECT_ROOT, "R/features/map_asa_metrics.R"), local = TRUE)
  source(file.path(PROJECT_ROOT, "R/utilities/data_provenance.R"), local = TRUE)

  collection <- readRDS(path)
  flat <- flatten_asa_collection(collection)

  if (is.null(flat) || !nrow(flat)) {
    write_log("ASA payload empty after flatten; using demo.")
    return(clean_demo_players(cfg))
  }

  dir_create_safe(cfg$paths$interim)
  readr::write_csv(flat, file.path(cfg$paths$interim, "asa_xgoals_flat.csv"))

  mapped <- standardize_asa_players(flat, cfg)
  readr::write_csv(mapped$players, file.path(cfg$paths$interim, "player_season_multi.csv"))
  readr::write_csv(mapped$teams, file.path(cfg$paths$interim, "teams_clean.csv"))

  # Default evaluation view written for DB load compatibility
  source(file.path(PROJECT_ROOT, "R/features/evaluation_periods.R"), local = TRUE)
  default_period <- cfg$project$evaluation_period_default %||% "blended"
  eval_df <- build_evaluation_table(mapped$players, cfg, period = default_period)
  readr::write_csv(eval_df, file.path(cfg$paths$interim, "player_season_clean.csv"))

  retrieved <- tryCatch({
    stamps <- purrr::map_chr(collection, \(x) x$retrieved_at %||% NA_character_)
    max(stamps, na.rm = TRUE)
  }, error = function(e) as.character(Sys.time()))

  prov <- make_live_provenance(
    cfg,
    n_players = dplyr::n_distinct(mapped$players$asa_player_id),
    seasons_present = sort(unique(mapped$players$season_year)),
    leagues_present = sort(unique(mapped$players$league_id)),
    retrieved_at = retrieved
  )
  write_provenance(cfg, prov)

  write_log(
    "ASA live clean complete: ", nrow(mapped$players), " player-seasons; ",
    nrow(eval_df), " players in default eval (", default_period, "). ",
    cutoff_label(prov)
  )
  invisible(mapped$players)
}

`%||%` <- function(a, b) if (!is.null(a)) a else b
