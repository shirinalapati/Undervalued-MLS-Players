# Feature engineering: shrinkage, percentiles, translated projections, role metrics

build_features <- function(players_df, cfg) {
  ensure_packages(c("dplyr", "tidyr"))
  tiers <- load_yaml("config/league_tiers.yml")
  roles <- load_yaml("config/role_weights.yml")
  min_minutes <- cfg$project$min_minutes %||% 450

  df <- players_df |>
    dplyr::filter(minutes >= min_minutes)

  # Position-league priors for EB shrinkage
  priors <- df |>
    dplyr::group_by(league_id, position_group) |>
    dplyr::summarise(
      dplyr::across(
        c(npxg_p90, xa_p90, pressures_p90, tackles_p90, interceptions_p90,
          progressive_passes_p90, progressive_carries_p90, goals_added_p90,
          shots_p90, crosses_p90),
        ~ mean(.x, na.rm = TRUE),
        .names = "prior_{.col}"
      ),
      .groups = "drop"
    )

  df <- df |>
    dplyr::left_join(priors, by = c("league_id", "position_group"))

  shrink_cols <- c("npxg_p90", "xa_p90", "pressures_p90", "tackles_p90", "interceptions_p90",
                   "progressive_passes_p90", "progressive_carries_p90", "goals_added_p90",
                   "shots_p90", "crosses_p90")

  for (col in shrink_cols) {
    prior_col <- paste0("prior_", col)
    out_col <- paste0("shrunk_", col)
    m0 <- tryCatch(shrinkage_m0_for(col, cfg), error = function(e) 600)
    df[[out_col]] <- empirical_bayes_shrink(df[[col]], df$minutes, df[[prior_col]], m0 = m0)
  }

  # Assumed league-strength adjustments (manual tier priors — not learned translations).
  # Safe order: raw → shrinkage → league-to-MLS translation → MLS position reference percentiles.
  get_factor <- function(league, family) {
    t <- tiers$tiers[[league]]
    if (is.null(t)) return(list(f = 0.7, u = 0.25))
    f <- switch(family,
      attack = t$translation_factor_attack,
      creation = t$translation_factor_creation,
      defense = t$translation_factor_defense,
      t$translation_factor_attack
    )
    list(f = f, u = t$uncertainty)
  }

  tf <- lapply(df$league_id, function(lg) get_factor(lg, "attack"))
  df$tf_attack <- vapply(seq_len(nrow(df)), function(i) get_factor(df$league_id[[i]], "attack")$f, numeric(1))
  df$tf_creation <- vapply(seq_len(nrow(df)), function(i) get_factor(df$league_id[[i]], "creation")$f, numeric(1))
  df$tf_defense <- vapply(seq_len(nrow(df)), function(i) get_factor(df$league_id[[i]], "defense")$f, numeric(1))
  df$tf_uncertainty <- vapply(seq_len(nrow(df)), function(i) get_factor(df$league_id[[i]], "attack")$u, numeric(1))
  df$tf_label <- "assumed_league_strength_adjustment"
  df$proj_npxg_p90 <- df$shrunk_npxg_p90 * df$tf_attack
  df$proj_xa_p90 <- df$shrunk_xa_p90 * df$tf_creation
  df$proj_pressures_p90 <- df$shrunk_pressures_p90 * df$tf_defense
  df$proj_gplus_p90 <- df$shrunk_goals_added_p90 * ((df$tf_attack + df$tf_creation + df$tf_defense) / 3)

  # Percentiles of translated rates by position vs MLS reference when available.
  pct_vs_mls_vec <- function(x, league_id, position_group) {
    out <- rep(NA_real_, length(x))
    for (pg in unique(position_group)) {
      idx <- which(position_group == pg)
      x_pg <- x[idx]
      lg_pg <- league_id[idx]
      mls_x <- x_pg[lg_pg == "mls" & is.finite(x_pg)]
      ok <- is.finite(x_pg)
      if (!any(ok)) next
      if (length(mls_x) >= 8) {
        out[idx[ok]] <- vapply(x_pg[ok], function(v) 100 * mean(mls_x <= v, na.rm = TRUE), numeric(1))
      } else {
        out[idx] <- percentile_rank(x_pg)
      }
    }
    out
  }

  df$pct_proj_npxg <- pct_vs_mls_vec(df$proj_npxg_p90, df$league_id, df$position_group)
  df$pct_proj_xa <- pct_vs_mls_vec(df$proj_xa_p90, df$league_id, df$position_group)
  df$pct_proj_press <- pct_vs_mls_vec(df$proj_pressures_p90, df$league_id, df$position_group)
  df$pct_proj_gplus <- pct_vs_mls_vec(df$proj_gplus_p90, df$league_id, df$position_group)

  df <- df |>
    dplyr::group_by(position_group) |>
    dplyr::mutate(
      pct_prog_pass = percentile_rank(shrunk_progressive_passes_p90),
      pct_prog_carry = percentile_rank(shrunk_progressive_carries_p90),
      pct_tackles = percentile_rank(shrunk_tackles_p90),
      pct_intercept = percentile_rank(shrunk_interceptions_p90),
      pct_shots = percentile_rank(shrunk_shots_p90),
      pct_crosses = percentile_rank(shrunk_crosses_p90),
      pct_aerial = percentile_rank(aerial_win_pct),
      pct_pass = percentile_rank(pass_completion_pct)
    ) |>
    dplyr::ungroup()

  # Role metric matrix. Fabricated constants → NA. Duplicate proxy aliases → NA
  # so missing-vs-missing is not treated as equal / zero-delta similarity.
  df <- df |>
    dplyr::mutate(
      pressures_p90_pct = pct_proj_press,
      defensive_actions_p90_pct = percentile_rank(shrunk_tackles_p90 + shrunk_interceptions_p90),
      turnovers_forced_p90_pct = NA_real_,
      progressive_runs_p90_pct = NA_real_,
      npxg_p90_pct = pct_proj_npxg,
      shots_p90_pct = pct_shots,
      ball_retention_pct = pct_pass,
      transition_involvement_pct = NA_real_,
      progressive_carries_p90_pct = pct_prog_carry,
      progressive_passes_received_p90_pct = pct_prog_pass,
      carries_into_final_third_p90_pct = pct_prog_carry,
      xa_p90_pct = pct_proj_xa,
      successful_dribbles_p90_pct = pct_prog_carry,
      tackles_p90_pct = pct_tackles,
      interceptions_p90_pct = pct_intercept,
      fouls_won_ratio_pct = NA_real_,
      progressive_passes_p90_pct = pct_prog_pass,
      pass_completion_under_pressure_pct = pct_pass,
      aerial_win_pct_pct = NA_real_,
      pass_completion_pct_pct = pct_pass,
      long_pass_completion_pct_pct = NA_real_,
      crosses_p90_pct = NA_real_,
      touches_att_third_p90_pct = NA_real_,
      metric_provenance_note = "pressures/tackles/progressions are g+-derived proxies, not ASA native summary fields"
    )

  attr(df, "roles") <- roles
  attr(df, "min_minutes") <- min_minutes
  df
}

score_role_fit <- function(df, role_id, roles_yaml = NULL) {
  score_role_fit_with_coverage(df, role_id, roles_yaml)$score
}

#' Role fit with coverage gates. Missing metrics stay NA (never filled with 50).
score_role_fit_with_coverage <- function(df, role_id, roles_yaml = NULL, spec = NULL) {
  roles_yaml <- roles_yaml %||% {
    if (exists("load_roles_config", mode = "function")) load_roles_config() else load_yaml("config/role_weights.yml")
  }
  spec <- spec %||% tryCatch(load_product_config(), error = function(e) load_model_spec())
  role <- roles_yaml$roles[[role_id]]
  if (is.null(role)) stop("Unknown role: ", role_id)

  weights <- unlist(role$metrics)
  cols <- paste0(names(weights), "_pct")
  w <- as.numeric(weights)
  names(w) <- cols

  n <- nrow(df)
  scores <- rep(NA_real_, n)
  coverage <- rep(0, n)

  for (i in seq_len(n)) {
    vals <- vapply(cols, function(col) {
      if (!col %in% names(df)) return(NA_real_)
      as.numeric(df[[col]][[i]])
    }, numeric(1))
    ok <- is.finite(vals)
    cov_i <- if (sum(abs(w)) < 1e-9) 0 else sum(abs(w[ok])) / sum(abs(w))
    coverage[i] <- cov_i
    unavailable_below <- spec$coverage$role_fit_unavailable_below %||% 0.5
    if (cov_i < unavailable_below || !any(ok)) {
      scores[i] <- NA_real_
    } else {
      ww <- w[ok] / sum(w[ok])
      scores[i] <- sum(ww * vals[ok])
    }
  }

  conf_cap <- if (is.null(spec)) {
    ifelse(coverage < 0.5, NA_character_,
           ifelse(coverage < 0.7, "low",
                  ifelse(coverage < 0.85, "medium", "high")))
  } else {
    coverage_max_confidence(coverage, spec)
  }

  list(score = scores, coverage = coverage, confidence_cap = conf_cap)
}

`%||%` <- function(a, b) if (!is.null(a)) a else b
