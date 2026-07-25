# Evaluation-period construction for 2026 live product.
# Periods:
#   ytd_2026  — 2026 season-to-date only (NOT full-season)
#   full_2025 — completed 2025 season baseline
#   blended   — 2026 YTD blended with 2025 prior by minutes reliability

METRIC_COLS <- c(
  "npxg_p90", "xa_p90", "shots_p90", "pressures_p90", "tackles_p90",
  "interceptions_p90", "progressive_passes_p90", "progressive_carries_p90",
  "goals_added_p90", "aerial_win_pct", "pass_completion_pct", "crosses_p90"
)

season_slice <- function(players_multi, season) {
  dplyr::filter(players_multi, season_year == as.integer(season))
}

development_trend <- function(players_multi) {
  ensure_packages("dplyr")
  wide <- players_multi |>
    dplyr::filter(season_year %in% c(2024, 2025, 2026)) |>
    dplyr::select(asa_player_id, player_id, season_year, goals_added_p90, minutes) |>
    dplyr::mutate(.w = pmax(minutes, 1)) |>
    dplyr::group_by(asa_player_id, season_year) |>
    dplyr::summarise(
      goals_added_p90 = weighted.mean(goals_added_p90, w = .w, na.rm = TRUE),
      minutes = sum(minutes, na.rm = TRUE),
      .groups = "drop"
    ) |>
    tidyr::pivot_wider(
      id_cols = asa_player_id,
      names_from = season_year,
      values_from = c(goals_added_p90, minutes),
      names_glue = "{.value}_{season_year}"
    )

  wide |>
    dplyr::mutate(
      yoy_delta = dplyr::case_when(
        !is.na(goals_added_p90_2025) & !is.na(goals_added_p90_2024) ~
          goals_added_p90_2025 - goals_added_p90_2024,
        !is.na(goals_added_p90_2026) & !is.na(goals_added_p90_2025) ~
          goals_added_p90_2026 - goals_added_p90_2025,
        TRUE ~ 0
      ),
      development_trend_note = dplyr::case_when(
        !is.na(goals_added_p90_2025) & !is.na(goals_added_p90_2024) ~
          "2024→2025 g+/90 change (longer-term trend)",
        !is.na(goals_added_p90_2026) & !is.na(goals_added_p90_2025) ~
          "2025→2026 YTD g+/90 change (short-term; 2026 incomplete)",
        TRUE ~ "Insufficient multi-season history for trend"
      )
    ) |>
    dplyr::select(asa_player_id, yoy_delta, development_trend_note)
}

attach_latest_salary <- function(base, salary_df) {
  if (is.null(salary_df) || !nrow(salary_df)) return(base)
  ensure_packages("dplyr")
  sal <- salary_df |>
    dplyr::group_by(asa_player_id) |>
    dplyr::slice_max(order_by = season_year, n = 1, with_ties = FALSE) |>
    dplyr::ungroup() |>
    dplyr::select(asa_player_id, salary, cost_tier)
  dplyr::left_join(base, sal, by = "asa_player_id", suffix = c("", "_sal")) |>
    dplyr::mutate(
      salary = dplyr::coalesce(salary_sal, salary),
      cost_tier = dplyr::coalesce(cost_tier_sal, cost_tier)
    ) |>
    dplyr::select(-dplyr::any_of(c("salary_sal", "cost_tier_sal")))
}

build_evaluation_table <- function(players_multi, cfg, period = "blended") {
  ensure_packages(c("dplyr", "tidyr"))
  period <- match.arg(period, c("ytd_2026", "full_2025", "blended"))

  s2026 <- season_slice(players_multi, 2026)
  s2025 <- season_slice(players_multi, 2025)
  s2024 <- season_slice(players_multi, 2024)

  trends <- development_trend(players_multi)

  # Identity spine: prefer 2026 roster presence, else 2025
  ids_2026 <- unique(s2026$asa_player_id)
  ids_2025 <- unique(s2025$asa_player_id)
  spine_ids <- unique(c(ids_2026, ids_2025))

  pick_identity <- function(df) {
    df |>
      dplyr::group_by(asa_player_id) |>
      dplyr::slice_max(order_by = minutes, n = 1, with_ties = FALSE) |>
      dplyr::ungroup()
  }

  id_2026 <- if (nrow(s2026)) pick_identity(s2026) else s2026
  id_2025 <- if (nrow(s2025)) pick_identity(s2025) else s2025

  identity <- dplyr::bind_rows(
    id_2026 |> dplyr::mutate(.src = "2026"),
    id_2025 |> dplyr::mutate(.src = "2025")
  ) |>
    dplyr::arrange(asa_player_id, .src) |>
    dplyr::group_by(asa_player_id) |>
    dplyr::slice_head(n = 1) |>
    dplyr::ungroup() |>
    dplyr::select(
      asa_player_id, player_id, display_name, normalized_name, birth_date, nationality,
      primary_position, is_domestic_player, age, league_id, team_id, position_group,
      tactical_role_primary, data_source
    )

  # Collapse multi-team season rows to player-season aggregates
  agg_season <- function(df, suffix) {
    if (!nrow(df)) {
      return(tibble::tibble(asa_player_id = character()))
    }
    metric_present <- intersect(METRIC_COLS, names(df))
    df <- df |>
      dplyr::mutate(.w = pmax(minutes, 1))

    # Build weighted means without clobbering the minutes weight vector mid-summarise
    out <- df |>
      dplyr::group_by(asa_player_id) |>
      dplyr::summarise(
        minutes = sum(minutes, na.rm = TRUE),
        salary = {
          s <- salary[is.finite(salary)]
          if (length(s)) max(s) else NA_real_
        },
        cost_tier = {
          c <- cost_tier[is.finite(cost_tier)]
          if (length(c)) as.integer(min(c)) else NA_integer_
        },
        .groups = "drop"
      )

    for (col in metric_present) {
      tmp <- df |>
        dplyr::group_by(asa_player_id) |>
        dplyr::summarise(
          val = stats::weighted.mean(.data[[col]], w = .w, na.rm = TRUE),
          .groups = "drop"
        )
      names(tmp)[2] <- col
      out <- dplyr::left_join(out, tmp, by = "asa_player_id")
    }

    out |>
      dplyr::rename_with(~ paste0(.x, "_", suffix), -asa_player_id)
  }

  a26 <- agg_season(s2026, "2026")
  a25 <- agg_season(s2025, "2025")

  joined <- identity |>
    dplyr::left_join(a26, by = "asa_player_id") |>
    dplyr::left_join(a25, by = "asa_player_id") |>
    dplyr::left_join(trends, by = "asa_player_id")

  m0 <- cfg$project$blend$prior_strength_minutes %||% 700
  min_share <- cfg$project$blend$min_ytd_share %||% 0.15
  max_share <- cfg$project$blend$max_ytd_share %||% 0.90

  if (identical(period, "ytd_2026")) {
    out <- joined |>
      dplyr::filter(!is.na(minutes_2026), minutes_2026 > 0) |>
      dplyr::mutate(
        evaluation_period = "ytd_2026",
        season_label = "2026 YTD (season-to-date — not full-season)",
        minutes = minutes_2026,
        minutes_share = pmin(pmax(minutes_2026 / 3060, 0.02), 1),
        blend_weight_2026 = 1,
        blend_weight_2025 = 0,
        data_reliability = dplyr::case_when(
          minutes_2026 >= 1200 ~ "medium",
          minutes_2026 >= 450 ~ "low-medium",
          TRUE ~ "low"
        )
      )
    for (col in METRIC_COLS) {
      out[[col]] <- out[[paste0(col, "_2026")]]
    }
  } else if (identical(period, "full_2025")) {
    out <- joined |>
      dplyr::filter(!is.na(minutes_2025), minutes_2025 > 0) |>
      dplyr::mutate(
        evaluation_period = "full_2025",
        season_label = "2025 full season (completed)",
        minutes = minutes_2025,
        minutes_share = pmin(pmax(minutes_2025 / 3060, 0.02), 1),
        blend_weight_2026 = 0,
        blend_weight_2025 = 1,
        data_reliability = dplyr::case_when(
          minutes_2025 >= 2000 ~ "high",
          minutes_2025 >= 900 ~ "medium",
          TRUE ~ "low"
        )
      )
    for (col in METRIC_COLS) {
      out[[col]] <- out[[paste0(col, "_2025")]]
    }
  } else {
    # Blended default
    out <- joined |>
      dplyr::filter(!is.na(minutes_2026) | !is.na(minutes_2025)) |>
      dplyr::mutate(
        minutes_2026 = dplyr::coalesce(minutes_2026, 0),
        minutes_2025 = dplyr::coalesce(minutes_2025, 0),
        blend_weight_2026 = dplyr::case_when(
          minutes_2026 <= 0 & minutes_2025 > 0 ~ 0,
          minutes_2026 > 0 & minutes_2025 <= 0 ~ 1,
          TRUE ~ pmin(max_share, pmax(min_share, minutes_2026 / (minutes_2026 + m0)))
        ),
        blend_weight_2025 = 1 - blend_weight_2026,
        evaluation_period = "blended",
        season_label = "2025+2026 season to date",
        minutes = pmax(minutes_2026, minutes_2025 * blend_weight_2025),
        minutes_share = pmin(pmax((minutes_2026 + 0.35 * minutes_2025) / 3060, 0.02), 1),
        data_reliability = dplyr::case_when(
          minutes_2026 >= 900 & minutes_2025 >= 900 ~ "high",
          minutes_2026 >= 450 | minutes_2025 >= 1200 ~ "medium",
          TRUE ~ "low"
        )
      )
    for (col in METRIC_COLS) {
      c26 <- out[[paste0(col, "_2026")]]
      c25 <- out[[paste0(col, "_2025")]]
      out[[col]] <- ifelse(
        is.na(c26) & is.na(c25),
        NA_real_,
        out$blend_weight_2026 * dplyr::coalesce(c26, c25) +
          out$blend_weight_2025 * dplyr::coalesce(c25, c26)
      )
    }
  }

  # Ensure season-suffixed salary columns exist even when a season slice was empty
  if (!"salary_2026" %in% names(out)) out$salary_2026 <- NA_real_
  if (!"salary_2025" %in% names(out)) out$salary_2025 <- NA_real_
  if (!"cost_tier_2026" %in% names(out)) out$cost_tier_2026 <- NA_integer_
  if (!"cost_tier_2025" %in% names(out)) out$cost_tier_2025 <- NA_integer_

  out <- out |>
    dplyr::mutate(
      season_year = cfg$project$product_season %||% 2026,
      salary = dplyr::coalesce(salary_2026, salary_2025),
      cost_tier = dplyr::coalesce(
        as.integer(cost_tier_2026),
        as.integer(cost_tier_2025)
      ),
      yoy_delta = dplyr::coalesce(yoy_delta, 0),
      stats_are_full_season = identical(period, "full_2025"),
      stats_are_ytd = identical(period, "ytd_2026") || identical(period, "blended")
    )

  # Rebuild stable player_id for product season view
  out$player_id <- paste0("asa_", out$asa_player_id, "_", out$league_id, "_eval_", period)

  # Drop helper season-suffixed metric columns from modeling frame
  drop_cols <- grep("_(2024|2025|2026)$", names(out), value = TRUE)
  out <- out[, setdiff(names(out), drop_cols), drop = FALSE]
  out
}

`%||%` <- function(a, b) if (!is.null(a)) a else b
