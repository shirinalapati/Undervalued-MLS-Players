# Data provenance + synthetic/live product guards

is_synthetic_dataset <- function(players_df = NULL, provenance = NULL) {
  # Prefer inspecting actual player rows — never trust a stale provenance flag alone.
  if (!is.null(players_df) && "data_source" %in% names(players_df)) {
    src <- players_df$data_source %||% ""
    if (any(grepl("^asa", src, ignore.case = TRUE), na.rm = TRUE) &&
        !any(grepl("^demo", src, ignore.case = TRUE), na.rm = TRUE)) {
      return(FALSE)
    }
    if (any(grepl("^demo", src, ignore.case = TRUE), na.rm = TRUE) ||
        all(grepl("^demo_", players_df$player_id %||% ""), na.rm = TRUE)) {
      return(TRUE)
    }
  }
  if (!is.null(provenance) && !is.null(provenance$is_synthetic)) {
    return(isTRUE(provenance$is_synthetic))
  }
  TRUE
}

#' Reconcile provenance with on-disk processed players; rewrite if mismatched.
reconcile_provenance_with_data <- function(cfg, players_df = NULL) {
  prov <- read_provenance(cfg)
  if (is.null(players_df)) {
    path <- file.path(cfg$paths$processed, "player_features_blended.csv")
    if (!file.exists(path)) path <- file.path(cfg$paths$interim, "player_season_multi.csv")
    if (file.exists(path)) {
      ensure_packages("readr")
      players_df <- readr::read_csv(path, show_col_types = FALSE, n_max = 5000)
    }
  }
  if (is.null(players_df) || !nrow(players_df)) return(prov)

  actual_syn <- is_synthetic_dataset(players_df, provenance = NULL)
  if (isTRUE(prov$is_synthetic) && !isTRUE(actual_syn)) {
    write_log("Provenance mismatch: flag was synthetic but data is live — rewriting live provenance.")
    seasons <- if ("season_year" %in% names(players_df)) sort(unique(players_df$season_year)) else list()
    leagues <- if ("league_id" %in% names(players_df)) sort(unique(players_df$league_id)) else list()
    n <- if ("asa_player_id" %in% names(players_df)) {
      dplyr::n_distinct(players_df$asa_player_id)
    } else {
      dplyr::n_distinct(players_df$player_id)
    }
    retrieved <- file.info(file.path(cfg$paths$raw, "asa_collection.rds"))$mtime %||% Sys.time()
    prov <- make_live_provenance(cfg, n, seasons, leagues, retrieved_at = retrieved)
    write_provenance(cfg, prov)
  }
  if (!isTRUE(prov$is_synthetic) && isTRUE(actual_syn)) {
    write_log("Provenance mismatch: flag was live but data looks synthetic — rewriting demo provenance.")
    prov <- make_demo_provenance(cfg)
    prov$n_players <- dplyr::n_distinct(players_df$player_id)
    write_provenance(cfg, prov)
  }
  prov
}

read_provenance <- function(cfg) {
  path <- cfg$paths$provenance %||% file.path(cfg$paths$processed, "data_provenance.json")
  if (!file.exists(path)) {
    return(list(
      is_synthetic = TRUE,
      data_cutoff_utc = NA_character_,
      data_cutoff_local = NA_character_,
      product_season = cfg$project$product_season %||% 2026,
      evaluation_period_default = cfg$project$evaluation_period_default %||% "blended",
      sources = list("synthetic_demo"),
      notes = "No provenance file — treat as synthetic demo."
    ))
  }
  ensure_packages("jsonlite")
  jsonlite::fromJSON(path, simplifyVector = TRUE)
}

write_provenance <- function(cfg, provenance) {
  ensure_packages("jsonlite")
  path <- cfg$paths$provenance %||% file.path(cfg$paths$processed, "data_provenance.json")
  dir_create_safe(dirname(path))
  jsonlite::write_json(provenance, path, pretty = TRUE, auto_unbox = TRUE)
  invisible(path)
}

make_live_provenance <- function(cfg, n_players, seasons_present, leagues_present, retrieved_at = Sys.time()) {
  list(
    is_synthetic = FALSE,
    product_season = cfg$project$product_season %||% 2026,
    evaluation_period_default = cfg$project$evaluation_period_default %||% "blended",
    data_cutoff_utc = format(as.POSIXct(retrieved_at, tz = "UTC"), "%Y-%m-%d %H:%M:%S UTC"),
    data_cutoff_local = format(as.POSIXct(retrieved_at), "%Y-%m-%d %H:%M:%S %Z"),
    performance_through = format(as.POSIXct(retrieved_at), "%Y-%m-%d"),
    mlspa_compensation_as_of = cfg$acquisition$asa$salary_as_of %||% "2026-04-16",
    official_roster_profile_as_of = cfg$acquisition$roster$snapshot_as_of %||% "not ingested — salary/minutes backbone only",
    transactions_through = cfg$acquisition$roster$transactions_through %||% "not systematically ingested",
    seasons_present = seasons_present,
    leagues_present = leagues_present,
    n_players = n_players,
    salary_season = cfg$acquisition$asa$salary_season %||% 2026,
    sources = list(
      "American Soccer Analysis API (itscalledsoccer)",
      "MLS player salaries via ASA / MLSPA public salary guide"
    ),
    notes = paste(
      "2026 metrics are season-to-date only and must not be labeled as full-season.",
      "2025 is the completed-season baseline prior.",
      "2024 is used only for longer-term development trends.",
      "League-tier factors are assumed league-strength adjustments until estimated from movers.",
      "Contribution index is unvalidated — not a precise projection."
    )
  )
}

make_demo_provenance <- function(cfg) {
  list(
    is_synthetic = TRUE,
    product_season = cfg$project$product_season %||% 2026,
    evaluation_period_default = cfg$project$evaluation_period_default %||% "blended",
    data_cutoff_utc = NA_character_,
    data_cutoff_local = NA_character_,
    seasons_present = list(cfg$project$product_season %||% 2026),
    leagues_present = list("mls", "mlsnp", "uslc"),
    n_players = NA_integer_,
    salary_season = NA_integer_,
    sources = list("synthetic_demo"),
    notes = "SYNTHETIC DEMO DATA — not suitable for genuine scouting reports or exports."
  )
}

source_cutoff_labels <- function(provenance) {
  if (isTRUE(provenance$is_synthetic)) {
    return(c("SYNTHETIC DEMO DATA — source cutoffs not applicable"))
  }
  c(
    paste0("Performance data through: ",
           provenance$performance_through %||% provenance$data_cutoff_local %||% "unknown"),
    paste0("MLSPA compensation as of: ",
           provenance$mlspa_compensation_as_of %||% provenance$salary_as_of %||% "unknown"),
    paste0("Official roster profile as of: ",
           provenance$official_roster_profile_as_of %||% "not ingested — salary/minutes backbone only"),
    paste0("Transactions incorporated through: ",
           provenance$transactions_through %||% "not systematically ingested")
  )
}

cutoff_label <- function(provenance) {
  if (isTRUE(provenance$is_synthetic)) {
    return("SYNTHETIC DEMO DATA — no live cutoff")
  }
  bits <- source_cutoff_labels(provenance)
  paste(bits, collapse = " · ")
}

evaluation_period_label <- function(period, cfg = NULL) {
  labels <- cfg$labels %||% list(
    ytd_2026 = "2026 YTD (season-to-date — not full-season)",
    full_2025 = "2025 full season (completed)",
    blended = "Blended recent performance (2026 YTD + 2025 prior)"
  )
  labels[[period]] %||% period
}

`%||%` <- function(a, b) if (!is.null(a)) a else b
