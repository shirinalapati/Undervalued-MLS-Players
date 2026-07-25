# 2026 MLS Value Index — scoring engine
# Primary Sporting Impact = position percentile of reliability-adjusted blended total G+/96.
# G+ components are explanatory only.

load_value_index_config <- function(path = NULL) {
  if (is.null(path)) path <- file.path(PROJECT_ROOT, "config", "value_index.yml")
  ensure_packages("yaml")
  yaml::read_yaml(path)
}

value_index_research_question <- function(vi_cfg = NULL) {
  vi_cfg <- vi_cfg %||% load_value_index_config()
  q <- vi_cfg$model$question %||%
    "Which current MLS players provide the most compensation-efficient position-adjusted on-ball impact?"
  trimws(gsub("\\s+", " ", as.character(q)))
}

value_index_counts <- function(scores) {
  list(
    n_players_evaluated = nrow(scores),
    n_official_eligible = sum(scores$official_eligible %in% TRUE, na.rm = TRUE)
  )
}

position_group_label <- function(pg, vi_cfg = NULL) {
  vi_cfg <- vi_cfg %||% load_value_index_config()
  lab <- vi_cfg$position_groups[[as.character(pg)]]$label
  if (is.null(lab) || !nzchar(lab)) return(as.character(pg))
  lab
}

gplus_component_cols <- function() {
  c(
    "goals_added_shooting_p90",
    "goals_added_passing_p90",
    "goals_added_receiving_p90",
    "goals_added_dribbling_p90",
    "goals_added_defending_p90",
    "goals_added_fouling_p90"
  )
}

value_metric_cols <- function() {
  c("goals_added_p90", gplus_component_cols())
}

#' Build MLS-only evaluation frame with G+ components retained.
build_mls_evaluation <- function(players_multi, cfg, period = "blended") {
  ensure_packages(c("dplyr", "tidyr"))
  period <- match.arg(period, c("ytd_2026", "full_2025", "blended"))
  metric_cols <- value_metric_cols()

  players_multi <- players_multi |>
    dplyr::filter(.data$league_id == "mls", !(.data$position_group %in% c("GK", "gk")))

  s2026 <- dplyr::filter(players_multi, season_year == 2026L)
  s2025 <- dplyr::filter(players_multi, season_year == 2025L)

  pick_identity <- function(df) {
    if (!nrow(df)) return(df)
    df |>
      dplyr::group_by(asa_player_id) |>
      dplyr::slice_max(order_by = minutes, n = 1, with_ties = FALSE) |>
      dplyr::ungroup()
  }

  id_2026 <- pick_identity(s2026)
  id_2025 <- pick_identity(s2025)
  identity <- dplyr::bind_rows(
    id_2026 |> dplyr::mutate(.src_rank = 1L),
    id_2025 |> dplyr::mutate(.src_rank = 2L)
  ) |>
    dplyr::arrange(asa_player_id, .src_rank) |>
    dplyr::group_by(asa_player_id) |>
    dplyr::slice_head(n = 1) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      # Mid-season transfers sometimes leave a printed multi-id string in older rows
      team_id = vapply(as.character(team_id), function(x) {
        if (grepl("^c\\(", x) || grepl(",", x)) {
          ids <- gsub('^c\\(|\\)$|"', "", x)
          ids <- trimws(unlist(strsplit(ids, ",")))
          ids <- ids[nzchar(ids)]
          if (length(ids)) ids[[1]] else NA_character_
        } else {
          x
        }
      }, character(1))
    ) |>
    dplyr::select(
      asa_player_id, player_id, display_name, normalized_name, birth_date, nationality,
      primary_position, is_domestic_player, age, league_id, team_id, position_group,
      data_source, dplyr::any_of(c("compensation_known", "salary_as_of", "salary_source"))
    )

  agg_season <- function(df, suffix) {
    if (!nrow(df)) {
      return(tibble::tibble(asa_player_id = character()))
    }
    metric_present <- intersect(metric_cols, names(df))
    df <- df |> dplyr::mutate(.w = pmax(minutes, 1))
    out <- df |>
      dplyr::group_by(asa_player_id) |>
      dplyr::summarise(
        minutes = sum(minutes, na.rm = TRUE),
        salary = {
          s <- salary[is.finite(salary) & salary > 0]
          if (length(s)) max(s) else NA_real_
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
    out |> dplyr::rename_with(~ paste0(.x, "_", suffix), -asa_player_id)
  }

  a26 <- agg_season(s2026, "2026")
  a25 <- agg_season(s2025, "2025")

  joined <- identity |>
    dplyr::left_join(a26, by = "asa_player_id") |>
    dplyr::left_join(a25, by = "asa_player_id")

  m0 <- cfg$project$blend$prior_strength_minutes %||% 700
  min_share <- cfg$project$blend$min_ytd_share %||% 0.15
  max_share <- cfg$project$blend$max_ytd_share %||% 0.90

  if (!"minutes_2026" %in% names(joined)) joined$minutes_2026 <- NA_real_
  if (!"minutes_2025" %in% names(joined)) joined$minutes_2025 <- NA_real_
  if (!"salary_2026" %in% names(joined)) joined$salary_2026 <- NA_real_
  if (!"salary_2025" %in% names(joined)) joined$salary_2025 <- NA_real_

  if (identical(period, "ytd_2026")) {
    out <- joined |>
      dplyr::filter(is.finite(minutes_2026), minutes_2026 > 0) |>
      dplyr::mutate(
        evaluation_period = "ytd_2026",
        season_label = "2026 season-to-date",
        minutes = minutes_2026,
        blend_weight_2026 = 1,
        blend_weight_2025 = 0,
        has_2025_prior = FALSE
      )
    for (col in metric_cols) {
      out[[col]] <- out[[paste0(col, "_2026")]]
    }
  } else if (identical(period, "full_2025")) {
    out <- joined |>
      dplyr::filter(is.finite(minutes_2025), minutes_2025 > 0) |>
      dplyr::mutate(
        evaluation_period = "full_2025",
        season_label = "2025 full season",
        minutes = minutes_2025,
        blend_weight_2026 = 0,
        blend_weight_2025 = 1,
        has_2025_prior = TRUE,
        minutes_2026 = dplyr::coalesce(minutes_2026, 0)
      )
    for (col in metric_cols) {
      out[[col]] <- out[[paste0(col, "_2025")]]
    }
  } else {
    out <- joined |>
      dplyr::filter(is.finite(minutes_2026) | is.finite(minutes_2025)) |>
      dplyr::mutate(
        minutes_2026 = dplyr::coalesce(minutes_2026, 0),
        minutes_2025 = dplyr::coalesce(minutes_2025, 0),
        has_2025_prior = minutes_2025 > 0,
        blend_weight_2026 = dplyr::case_when(
          minutes_2026 <= 0 & minutes_2025 > 0 ~ 0,
          minutes_2026 > 0 & minutes_2025 <= 0 ~ 1,
          TRUE ~ pmin(max_share, pmax(min_share, minutes_2026 / (minutes_2026 + m0)))
        ),
        blend_weight_2025 = 1 - blend_weight_2026,
        evaluation_period = "blended",
        season_label = "2025+2026 season to date",
        minutes = minutes_2026 + minutes_2025 * blend_weight_2025
      )
    for (col in metric_cols) {
      c26 <- out[[paste0(col, "_2026")]]
      c25 <- out[[paste0(col, "_2025")]]
      out[[col]] <- dplyr::case_when(
        is.na(c26) & is.na(c25) ~ NA_real_,
        !is.finite(c26) & is.finite(c25) ~ c25,
        is.finite(c26) & !is.finite(c25) ~ c26,
        TRUE ~ out$blend_weight_2026 * c26 + out$blend_weight_2025 * c25
      )
    }
  }

  out <- out |>
    dplyr::mutate(
      season_year = cfg$project$product_season %||% 2026,
      guaranteed_compensation = dplyr::coalesce(salary_2026, salary_2025),
      compensation_known = is.finite(guaranteed_compensation) & guaranteed_compensation > 0,
      minutes_2026 = dplyr::coalesce(minutes_2026, 0),
      minutes_2025 = dplyr::coalesce(minutes_2025, 0)
    )

  drop_metric <- c(paste0(metric_cols, "_2026"), paste0(metric_cols, "_2025"))
  out <- out[, setdiff(names(out), drop_metric), drop = FALSE]
  out$player_id <- paste0("asa_", out$asa_player_id, "_mls_", period)
  out
}

attach_team_names <- function(df, teams_df) {
  if (is.null(teams_df) || !nrow(teams_df)) {
    df$club <- as.character(df$team_id)
    return(df)
  }
  teams_df <- teams_df |>
    dplyr::distinct(team_id, .keep_all = TRUE) |>
    dplyr::mutate(club = dplyr::coalesce(team_name, team_short, as.character(team_id)))
  dplyr::left_join(
    df,
    teams_df |> dplyr::select(team_id, club),
    by = "team_id"
  ) |>
    dplyr::mutate(club = dplyr::coalesce(club, as.character(team_id)))
}

#' Core value scoring for one evaluation period (MLS-only frame).
score_mls_value_index <- function(eval_df, vi_cfg = NULL, model_version = NULL) {
  ensure_packages("dplyr")
  vi_cfg <- vi_cfg %||% load_value_index_config()
  model_version <- model_version %||% (vi_cfg$model$version %||% "1.1.0")
  m0 <- vi_cfg$shrinkage$prior_strength_minutes %||% 600
  p96 <- vi_cfg$shrinkage$p96_factor %||% (96 / 90)
  min_official <- vi_cfg$eligibility$min_minutes_2026_official %||% 450
  min_impact_label <- vi_cfg$eligibility$min_sporting_impact_for_undervalued_label %||% 55
  labs <- vi_cfg$labels

  df <- eval_df |>
    dplyr::filter(
      league_id == "mls",
      !is.na(position_group),
      nzchar(as.character(position_group)),
      !(position_group %in% c("GK", "gk"))
    )

  if (!"has_2025_prior" %in% names(df)) {
    df$has_2025_prior <- is.finite(df$minutes_2025) & df$minutes_2025 > 0
  }
  if (!"minutes_2026" %in% names(df)) df$minutes_2026 <- df$minutes
  if (!"minutes_2025" %in% names(df)) df$minutes_2025 <- 0

  priors <- df |>
    dplyr::group_by(position_group) |>
    dplyr::summarise(
      prior_goals_added_p90 = mean(goals_added_p90, na.rm = TRUE),
      .groups = "drop"
    )

  df <- df |>
    dplyr::left_join(priors, by = "position_group") |>
    dplyr::mutate(
      prior_goals_added_p90 = dplyr::coalesce(prior_goals_added_p90, 0),
      metric_coverage = ifelse(is.finite(goals_added_p90), 1, 0),
      adjusted_goals_added_p90 = ifelse(
        is.finite(goals_added_p90),
        empirical_bayes_shrink(
          goals_added_p90, pmax(minutes, 1), prior_goals_added_p90, m0 = m0
        ),
        NA_real_
      ),
      adjusted_goals_added_p96 = adjusted_goals_added_p90 * p96
    )

  for (col in gplus_component_cols()) {
    if (!col %in% names(df)) df[[col]] <- NA_real_
    p96_col <- sub("_p90$", "_p96", col)
    df[[p96_col]] <- ifelse(is.finite(df[[col]]), df[[col]] * p96, NA_real_)
    pct_col <- paste0("pct_", gsub("_p90$", "", col))
    df <- df |>
      dplyr::group_by(position_group) |>
      dplyr::mutate(!!pct_col := percentile_rank(.data[[col]])) |>
      dplyr::ungroup()
  }

  df <- df |>
    dplyr::group_by(position_group) |>
    dplyr::mutate(
      sporting_impact = dplyr::if_else(
        metric_coverage >= 0.5 & is.finite(adjusted_goals_added_p96),
        percentile_rank(adjusted_goals_added_p96),
        NA_real_
      )
    ) |>
    dplyr::ungroup()

  df <- df |>
    dplyr::mutate(
      compensation = guaranteed_compensation,
      compensation_known = isTRUE(compensation_known) |
        (is.finite(compensation) & compensation > 0)
    )

  df$compensation_percentile <- NA_real_
  df$compensation_percentile_league <- NA_real_
  known <- which(df$compensation_known)
  if (length(known)) {
    df$compensation_percentile_league[known] <- percentile_rank(df$compensation[known])
    for (pg in unique(df$position_group[known])) {
      idx <- known[df$position_group[known] == pg]
      df$compensation_percentile[idx] <- percentile_rank(df$compensation[idx])
    }
  }

  med <- df |>
    dplyr::filter(compensation_known) |>
    dplyr::group_by(position_group) |>
    dplyr::summarise(
      position_median_compensation = stats::median(compensation, na.rm = TRUE),
      .groups = "drop"
    )
  df <- dplyr::left_join(df, med, by = "position_group")

  df <- df |>
    dplyr::mutate(
      value_surplus = dplyr::if_else(
        is.finite(sporting_impact) & is.finite(compensation_percentile),
        sporting_impact - compensation_percentile,
        NA_real_
      ),
      official_eligible = compensation_known &
        is.finite(sporting_impact) &
        metric_coverage >= 0.5 &
        minutes_2026 >= min_official,
      small_sample_watchlist = compensation_known &
        is.finite(sporting_impact) &
        metric_coverage >= 0.5 &
        minutes_2026 > 0 &
        minutes_2026 < min_official
    )

  df$undervaluation_score <- NA_real_
  elig <- which(df$official_eligible)
  if (length(elig)) {
    for (pg in unique(df$position_group[elig])) {
      idx <- elig[df$position_group[elig] == pg]
      df$undervaluation_score[idx] <- percentile_rank(df$value_surplus[idx])
    }
  }

  df <- df |>
    dplyr::mutate(
      data_confidence = dplyr::case_when(
        !compensation_known | !is.finite(sporting_impact) | metric_coverage < 0.5 ~
          "Insufficient",
        minutes_2026 < min_official ~ "Low",
        minutes_2026 < 900 & !has_2025_prior ~ "Low",
        minutes_2026 < 900 ~ "Medium",
        has_2025_prior & minutes_2026 >= 900 ~ "High",
        minutes_2026 >= 1200 ~ "High",
        TRUE ~ "Medium"
      ),
      model_uncertainty = clip(
        0.45 * dplyr::case_when(
          minutes_2026 < 450 ~ 80,
          minutes_2026 < 900 ~ 55,
          minutes_2026 < 1500 ~ 35,
          TRUE ~ 20
        ) +
          0.25 * (100 * (1 - metric_coverage)) +
          0.20 * dplyr::if_else(has_2025_prior, 15, 55) +
          0.10 * 20,
        0, 100
      ),
      model_confidence = 100 - model_uncertainty,
      value_label = dplyr::case_when(
        !compensation_known | !is.finite(sporting_impact) | metric_coverage < 0.5 ~
          "Insufficient Evidence",
        small_sample_watchlist ~ "Small-Sample Watchlist",
        official_eligible &
          is.finite(value_surplus) &
          value_surplus >= (labs$elite_value$min_value_surplus %||% 25) &
          is.finite(undervaluation_score) &
          undervaluation_score >= (labs$elite_value$min_undervaluation_score %||% 90) &
          sporting_impact >= (labs$elite_value$min_sporting_impact %||% 70) &
          data_confidence %in% (labs$elite_value$min_confidence %||% c("Medium", "High")) ~
          "Elite Value",
        official_eligible &
          is.finite(value_surplus) &
          value_surplus >= (labs$strong_value$min_value_surplus %||% 15) &
          is.finite(undervaluation_score) &
          undervaluation_score >= (labs$strong_value$min_undervaluation_score %||% 75) &
          sporting_impact >= (labs$strong_value$min_sporting_impact %||% 60) &
          data_confidence %in% (labs$strong_value$min_confidence %||% c("Medium", "High")) ~
          "Strong Value",
        official_eligible &
          is.finite(value_surplus) &
          value_surplus >= (labs$undervalued$min_value_surplus %||% 5) &
          is.finite(undervaluation_score) &
          undervaluation_score >= (labs$undervalued$min_undervaluation_score %||% 60) &
          sporting_impact >= (labs$undervalued$min_sporting_impact %||% min_impact_label) ~
          "Undervalued",
        official_eligible &
          is.finite(value_surplus) &
          value_surplus <= (labs$below_expected_by_model$max_value_surplus %||% -15) ~
          "Below Expected Value by Current Model",
        # Fair Value: surplus in (-15, 5), or fails impact / confidence floors for higher labels
        official_eligible ~ "Fair Value",
        TRUE ~ "Insufficient Evidence"
      ),
      position_label = vapply(
        as.character(position_group), position_group_label, character(1), vi_cfg = vi_cfg
      ),
      model_version = model_version,
      calculation_timestamp = format(Sys.time(), tz = "UTC", usetz = TRUE)
    )

  # Position Rank: resets within each position group among official eligible players
  df$undervaluation_rank <- NA_integer_
  df$position_rank <- NA_integer_
  if (length(elig)) {
    for (pg in unique(df$position_group[elig])) {
      idx <- elig[df$position_group[elig] == pg]
      ord <- order(
        -df$undervaluation_score[idx],
        -df$value_surplus[idx],
        df$display_name[idx]
      )
      ranks <- integer(length(idx))
      ranks[ord] <- seq_along(idx)
      df$undervaluation_rank[idx] <- ranks
      df$position_rank[idx] <- ranks
    }
  }

  # Display Rank: sequential across the full official eligible pool (never resets by position)
  df$display_rank <- NA_integer_
  if (length(elig)) {
    ord <- order(
      -df$undervaluation_score[elig],
      -df$value_surplus[elig],
      df$display_name[elig]
    )
    ranks <- integer(length(elig))
    ranks[ord] <- seq_along(elig)
    df$display_rank[elig] <- ranks
  }

  df
}

`%||%` <- function(a, b) if (!is.null(a)) a else b
