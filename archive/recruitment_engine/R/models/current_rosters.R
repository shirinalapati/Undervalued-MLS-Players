# Current MLS roster membership (2026) from MLSPA/ASA salary guide + YTD activity.
# Do NOT infer "current roster" from blended/historical team tags alone.

load_mls_salary_guide <- function(cfg = NULL) {
  cfg <- cfg %||% tryCatch(load_config(), error = function(e) NULL)
  season <- cfg$acquisition$asa$salary_season %||% 2026
  path <- file.path(cfg$paths$raw %||% "data/raw", "cache", paste0("mls_salaries_", season, ".rds"))
  if (!file.exists(path)) return(NULL)
  obj <- readRDS(path)
  sal <- if (is.list(obj) && !is.null(obj$salaries)) obj$salaries else obj
  if (!is.data.frame(sal) || !nrow(sal)) return(NULL)
  sal
}

#' Build current-roster membership for all MLS clubs from the 2026 salary guide.
#' Active = has 2026 MLS minutes for that same team (excludes pure loans / departed).
build_current_mls_rosters <- function(cfg = NULL, team_map = NULL, multi_path = NULL,
                                      scores_df = NULL, include_gk = FALSE) {
  ensure_packages(c("dplyr", "tibble"))
  cfg <- cfg %||% load_config()
  team_map <- team_map %||% load_club_team_map()
  sal <- load_mls_salary_guide(cfg)
  if (is.null(sal)) {
    return(tibble::tibble(
      club_id = character(), asa_player_id = character(), team_id = character(),
      roster_status = character(), minutes_2026 = numeric()
    ))
  }

  # Reverse map ASA team_id → club_id
  team_to_club <- list()
  for (cid in names(team_map$clubs)) {
    for (tid in as.character(unlist(team_map$clubs[[cid]]$asa_team_ids %||% list()))) {
      team_to_club[[tid]] <- cid
    }
  }

  roster <- tibble::tibble(
    asa_player_id = as.character(sal$player_id),
    team_id = as.character(sal$team_id),
    salary_usd = as.numeric(dplyr::coalesce(
      suppressWarnings(as.numeric(sal$guaranteed_compensation)),
      suppressWarnings(as.numeric(sal$base_salary))
    )),
    salary_position = if ("position" %in% names(sal)) as.character(sal$position) else NA_character_
  ) |>
    dplyr::filter(!is.na(asa_player_id), !is.na(team_id), nzchar(team_id)) |>
    dplyr::mutate(
      club_id = vapply(team_id, function(t) team_to_club[[t]] %||% NA_character_, character(1))
    ) |>
    dplyr::filter(!is.na(club_id))

  # 2026 YTD minutes by player × team (true activity for that club)
  multi_path <- multi_path %||% file.path(cfg$paths$interim, "player_season_multi.csv")
  minutes_2026 <- tibble::tibble(asa_player_id = character(), team_id = character(), minutes_2026 = numeric())
  if (file.exists(multi_path)) {
    multi <- readr::read_csv(multi_path, show_col_types = FALSE)
    minutes_2026 <- multi |>
      dplyr::filter(
        .data$season_year == 2026,
        .data$league_id == "mls",
        is.finite(.data$minutes),
        .data$minutes > 0,
        !grepl("^c\\(", .data$team_id)
      ) |>
      dplyr::group_by(.data$asa_player_id, .data$team_id) |>
      dplyr::summarise(minutes_2026 = sum(.data$minutes, na.rm = TRUE), .groups = "drop")
  }

  roster <- roster |>
    dplyr::left_join(minutes_2026, by = c("asa_player_id", "team_id")) |>
    dplyr::mutate(
      minutes_2026 = dplyr::coalesce(.data$minutes_2026, 0),
      roster_status = dplyr::case_when(
        .data$minutes_2026 > 0 ~ "active_2026",
        TRUE ~ "on_books_no_2026_minutes" # loaned, injured, or not yet used
      )
    )

  # Attach display identity / scores from evaluation frame when provided
  if (!is.null(scores_df) && nrow(scores_df)) {
    id_cols <- c(
      "asa_player_id", "display_name", "age", "position_group", "tactical_role_primary",
      "league_id", "score_projected_mls", "score_role_fit", "score_development",
      "score_risk", "score_feasibility", "score_financial_value", "score_overall",
      "minutes", "salary", "cost_tier", "pct_proj_press", "pct_prog_pass",
      "pct_prog_carry", "pct_tackles", "pct_intercept", "pct_pass",
      "is_domestic_player", "player_id", "role_id", "confidence", "minutes_share",
      "data_reliability"
    )
    id_cols <- intersect(id_cols, names(scores_df))
    # One identity row per player (prefer MLS / highest minutes)
    ident <- scores_df |>
      dplyr::filter(.data$league_id == "mls" | is.na(.data$league_id)) |>
      dplyr::group_by(.data$asa_player_id) |>
      dplyr::slice_max(order_by = dplyr::coalesce(.data$minutes, 0), n = 1, with_ties = FALSE) |>
      dplyr::ungroup() |>
      dplyr::select(dplyr::any_of(id_cols))
    # Drop score team_id so salary team_id wins
    roster <- dplyr::left_join(roster, ident, by = "asa_player_id")
  }

  if (!isTRUE(include_gk)) {
    roster <- roster |>
      dplyr::filter(
        is.na(.data$salary_position) | toupper(.data$salary_position) != "GK",
        is.na(.data$position_group) | .data$position_group != "GK"
      )
  }

  # Prefer salary from guide
  if ("salary" %in% names(roster)) {
    roster <- roster |>
      dplyr::mutate(salary = dplyr::coalesce(.data$salary_usd, .data$salary))
  } else {
    roster$salary <- roster$salary_usd
  }

  roster |>
    dplyr::arrange(.data$club_id, dplyr::desc(.data$minutes_2026), .data$display_name)
}

write_current_mls_rosters <- function(cfg, scores_df = NULL) {
  rost <- build_current_mls_rosters(cfg, scores_df = scores_df)
  out <- file.path(cfg$paths$processed, "mls_current_rosters.csv")
  dir_create_safe(dirname(out))
  readr::write_csv(rost, out)
  write_log(
    "Wrote current MLS rosters: ", nrow(rost), " player-rows across ",
    dplyr::n_distinct(rost$club_id), " clubs → ", out
  )
  invisible(rost)
}

#' Club roster for UI/needs: default active 2026 only (exclude loans / departed ghosts).
club_roster_players <- function(players_df, club_id, team_map = NULL, min_minutes = 1,
                                active_only = TRUE, cfg = NULL) {
  ensure_packages("dplyr")
  cfg <- cfg %||% tryCatch(load_config(), error = function(e) NULL)
  team_map <- team_map %||% load_club_team_map()

  processed <- NULL
  rost_path <- file.path(cfg$paths$processed %||% "data/processed", "mls_current_rosters.csv")
  if (file.exists(rost_path)) {
    processed <- readr::read_csv(rost_path, show_col_types = FALSE)
  }

  if (!is.null(processed) && nrow(processed)) {
    out <- processed |>
      dplyr::filter(.data$club_id == .env$club_id)
    if (isTRUE(active_only)) {
      out <- dplyr::filter(out, .data$roster_status == "active_2026")
    }
    # Ensure score columns exist by joining live players_df when needed
    if (!is.null(players_df) && nrow(players_df) && !"score_projected_mls" %in% names(out)) {
      # already should have scores from build; if missing join
    }
    if (!is.null(players_df) && nrow(players_df) && "score_projected_mls" %in% names(players_df)) {
      score_cols <- setdiff(names(players_df), c("team_id", "league_id", "salary", "minutes"))
      # Prefer role-neutral: take best minutes row per player from players_df
      sc <- players_df |>
        dplyr::group_by(.data$asa_player_id) |>
        dplyr::slice_max(order_by = dplyr::coalesce(.data$minutes, 0), n = 1, with_ties = FALSE) |>
        dplyr::ungroup()
      keep <- unique(c("asa_player_id", intersect(names(sc), setdiff(names(players_df), names(out)))))
      # Always refresh key score fields from current evaluation period
      refresh <- c(
        "display_name", "age", "position_group", "tactical_role_primary",
        "score_projected_mls", "score_role_fit", "score_development", "score_risk",
        "score_feasibility", "score_financial_value", "score_overall", "minutes",
        "cost_tier", "pct_proj_press", "pct_prog_pass", "pct_prog_carry",
        "pct_tackles", "pct_intercept", "pct_pass", "is_domestic_player",
        "player_id", "role_id", "confidence", "minutes_share", "data_reliability"
      )
      refresh <- intersect(refresh, names(sc))
      sc2 <- sc[, c("asa_player_id", refresh), drop = FALSE]
      # drop overlapping refresh cols from out first
      out <- out[, setdiff(names(out), setdiff(refresh, "asa_player_id")), drop = FALSE]
      out <- dplyr::left_join(out, sc2, by = "asa_player_id")
    }
    out <- out |>
      dplyr::filter(dplyr::coalesce(.data$minutes_2026, .data$minutes, 0) >= min_minutes | !isTRUE(active_only))
    return(dplyr::arrange(out, dplyr::desc(dplyr::coalesce(.data$minutes_2026, .data$minutes, 0))))
  }

  # Fallback (legacy): ASA team_id filter — prefer 2026 YTD file when passed
  ids <- asa_team_ids_for_club(club_id, team_map)
  if (!length(ids) || is.null(players_df) || !nrow(players_df)) {
    return(players_df[0, , drop = FALSE])
  }
  players_df |>
    dplyr::filter(.data$team_id %in% ids, .data$league_id == "mls") |>
    dplyr::group_by(.data$asa_player_id) |>
    dplyr::slice_max(order_by = .data$minutes, n = 1, with_ties = FALSE) |>
    dplyr::ungroup() |>
    dplyr::filter(.data$minutes >= min_minutes) |>
    dplyr::arrange(dplyr::desc(.data$minutes))
}
