# Component scoring + overall ranking (accuracy-first heuristic prototype).
# Labels and weights: config/model_spec.yml — do not hardcode competing formulas elsewhere.

# Component scoring + overall ranking (accuracy-first hybrid prototype).
# Learned outcomes when artifacts exist; configurable decision layer; transparent rules.
# Labels/weights: config/model_spec.yml

# Heuristic role-specific blend — used ONLY as fallback until contribution artifact is fitted.
score_contribution_index_heuristic <- function(df, role_id, spec = NULL) {
  spec <- spec %||% load_model_spec()
  weights <- contribution_weights_for_role(role_id, spec)

  resolve_col <- function(key) {
    candidates <- c(
      paste0(key, "_pct"),
      paste0("pct_", key),
      key,
      if (identical(key, "pct_npxg")) "pct_proj_npxg",
      if (identical(key, "pct_xa")) "pct_proj_xa",
      if (identical(key, "goals_added_shooting")) "pct_gplus_shooting",
      if (identical(key, "goals_added_passing")) "pct_gplus_passing",
      if (identical(key, "goals_added_receiving")) "pct_gplus_receiving",
      if (identical(key, "goals_added_dribbling")) "pct_gplus_dribbling",
      if (identical(key, "goals_added_defending")) "pct_gplus_defending",
      if (identical(key, "goals_added_fouling")) "pct_gplus_fouling"
    )
    candidates <- unique(unlist(candidates))
    hit <- candidates[candidates %in% names(df)]
    if (!length(hit)) return(NA_character_)
    hit[[1]]
  }

  cols <- vapply(names(weights), resolve_col, character(1))
  available <- !is.na(cols)
  if (!any(available)) {
    raw <- 0.40 * df$pct_proj_npxg + 0.35 * df$pct_proj_xa + 0.25 * df$pct_proj_press
    coverage <- rep(0.4, nrow(df))
  } else {
    w <- as.numeric(weights[available])
    mat <- matrix(NA_real_, nrow = nrow(df), ncol = sum(available))
    for (j in seq_along(cols[available])) {
      mat[, j] <- as.numeric(df[[cols[available][[j]]]])
    }
    abs_w <- abs(w)
    total_abs <- sum(abs_w)
    obs <- is.finite(mat)
    cov_num <- as.numeric(obs %*% abs_w)
    coverage <- cov_num / max(total_abs, 1e-9)

    raw <- vapply(seq_len(nrow(df)), function(i) {
      ok <- is.finite(mat[i, ])
      if (!any(ok)) return(NA_real_)
      ww <- w[ok]
      vals <- mat[i, ok]
      pos <- ww > 0
      if (any(pos)) {
        base <- sum(ww[pos] * vals[pos]) / sum(ww[pos])
      } else {
        base <- 50
      }
      if (any(!pos)) {
        pen <- sum(abs(ww[!pos]) * vals[!pos]) / max(sum(abs(ww[!pos])), 1e-9)
        base <- base - 0.15 * (pen - 50)
      }
      base
    }, numeric(1))
  }

  minutes_factor <- clip(df$minutes / 2500, 0.7, 1)
  score <- ifelse(is.finite(raw), clip(raw * minutes_factor, 0, 100), NA_real_)
  list(score = score, coverage = coverage)
}

# Back-compat alias
score_contribution_index <- function(df, role_id, spec = NULL) {
  score_contribution_index_heuristic(df, role_id, spec)
}

score_feasibility <- function(df) {
  cost_score <- dplyr::case_when(
    !is.finite(df$cost_tier) ~ 45,
    df$cost_tier == 1 ~ 95,
    df$cost_tier == 2 ~ 80,
    df$cost_tier == 3 ~ 55,
    df$cost_tier == 4 ~ 30,
    TRUE ~ 10
  )
  league_score <- dplyr::case_when(
    df$league_id == "uslc" ~ 85,
    df$league_id == "mlsnp" ~ 80,
    df$league_id == "mls" ~ 60,
    TRUE ~ 50
  )
  minutes_score <- ifelse(df$league_id == "mls" & df$minutes_share > 0.75, 45, 75)
  intl_status <- if ("intl_roster_status" %in% names(df)) df$intl_roster_status else rep(NA_character_, nrow(df))
  domestic_score <- dplyr::case_when(
    tolower(as.character(intl_status)) %in% c("domestic", "homegrown") ~ 80,
    tolower(as.character(intl_status)) %in% c("international") ~ 55,
    TRUE ~ 60
  )
  0.35 * cost_score + 0.30 * league_score + 0.20 * minutes_score + 0.15 * domestic_score
}

score_development <- function(df, spec = NULL, cfg = NULL) {
  # Age is a future-development input only. Prefer learned peaks when artifact exists.
  peaks <- tryCatch(
    age_peak_hybrid(df$position_group, spec, cfg),
    error = function(e) age_peak_for_position(df$position_group, spec)
  )
  age <- as.numeric(df$age)
  score <- 100 * exp(-0.5 * ((age - (peaks - 3)) / 4.5)^2)
  above <- pmax(0, age - peaks)
  age_ups <- clip(score - pmin(45, above * 6), 5, 95)
  yoy <- ifelse(is.finite(df$yoy_delta), scale_0_100(df$yoy_delta, na_fill = NA_real_), NA_real_)
  minutes_traj <- scale_0_100(df$minutes, na_fill = NA_real_)
  out <- vapply(seq_len(nrow(df)), function(i) {
    bits <- c(age_ups[[i]])
    wts <- c(0.55)
    if (is.finite(yoy[[i]])) {
      bits <- c(bits, yoy[[i]]); wts <- c(wts, 0.25)
    }
    if (is.finite(minutes_traj[[i]])) {
      bits <- c(bits, minutes_traj[[i]]); wts <- c(wts, 0.20)
    }
    sum(bits * wts) / sum(wts)
  }, numeric(1))
  clip(out, 0, 100)
}

score_compensation_adjusted_value <- function(df, contribution_score) {
  contrib <- contribution_score
  cost <- dplyr::case_when(
    !is.finite(df$cost_tier) ~ NA_real_,
    df$cost_tier == 1 ~ 20,
    df$cost_tier == 2 ~ 40,
    df$cost_tier == 3 ~ 60,
    df$cost_tier == 4 ~ 80,
    TRUE ~ 95
  )
  ifelse(is.finite(cost) & is.finite(contrib), clip(contrib - cost + 50, 0, 100), NA_real_)
}

score_model_uncertainty <- function(df, role_coverage = NULL) {
  sample_u <- dplyr::case_when(
    df$minutes < 700 ~ 80,
    df$minutes < 1200 ~ 55,
    df$minutes < 2000 ~ 35,
    TRUE ~ 20
  )
  translation_u <- 100 * (df$tf_uncertainty %||% 0.15)
  cov <- role_coverage
  if (is.null(cov)) {
    cov <- if ("role_metric_coverage" %in% names(df)) df$role_metric_coverage else rep(1, nrow(df))
  }
  missing_u <- clip(100 * (1 - cov), 0, 100)
  clip(0.40 * sample_u + 0.35 * translation_u + 0.25 * missing_u, 0, 100)
}

score_sporting_volatility <- function(df) {
  yoy_vol <- ifelse(is.finite(df$yoy_delta), clip(abs(df$yoy_delta) * 80, 10, 90), 45)
  age_vol <- ifelse(df$age >= 32 | df$age <= 19, 65, 30)
  clip(0.6 * yoy_vol + 0.4 * age_vol, 0, 100)
}

score_acquisition_complexity <- function(df) {
  cost_c <- dplyr::case_when(
    !is.finite(df$cost_tier) ~ 55,
    df$cost_tier >= 4 ~ 70,
    df$cost_tier >= 3 ~ 45,
    TRUE ~ 25
  )
  intl <- if ("intl_roster_status" %in% names(df)) df$intl_roster_status else rep(NA_character_, nrow(df))
  intl_c <- dplyr::case_when(
    tolower(as.character(intl)) == "international" ~ 60,
    tolower(as.character(intl)) %in% c("domestic", "homegrown") ~ 25,
    TRUE ~ 40
  )
  pathway_c <- dplyr::case_when(
    df$league_id == "mls" & df$minutes_share > 0.7 ~ 65,
    df$league_id == "mls" ~ 45,
    df$league_id == "uslc" ~ 40,
    df$league_id == "mlsnp" ~ 35,
    TRUE ~ 50
  )
  clip(0.40 * cost_c + 0.30 * intl_c + 0.30 * pathway_c, 0, 100)
}

compute_component_scores <- function(df, role_id, cfg) {
  ensure_packages("dplyr")
  spec <- load_model_spec()
  out <- df
  out$role_id <- role_id

  role_res <- score_role_fit_with_coverage(out, role_id)
  out$score_role_fit <- role_res$score
  out$role_metric_coverage <- role_res$coverage
  out$role_fit_confidence_cap <- role_res$confidence_cap

  contrib <- tryCatch(
    score_contribution_hybrid(out, role_id, spec, cfg),
    error = function(e) {
      h <- score_contribution_index_heuristic(out, role_id, spec)
      list(
        score = h$score, coverage = h$coverage, source = "heuristic_fallback",
        prediction_raw = NA_real_, model_type = "role_blend_heuristic"
      )
    }
  )
  if (is.null(contrib$score)) {
    h <- score_contribution_index_heuristic(out, role_id, spec)
    contrib <- list(score = h$score, coverage = h$coverage, source = "heuristic_fallback")
  }
  out$score_projected_mls <- contrib$score
  out$score_contribution_index <- contrib$score
  out$contribution_coverage <- contrib$coverage
  out$contribution_source <- contrib$source %||% "heuristic_fallback"

  out <- tryCatch(score_feasibility_hybrid(out, cfg, spec), error = function(e) {
    out$score_feasibility <- score_feasibility(out)
    out$feasibility_source <- "heuristic_rules"
    out
  })
  out$score_development <- score_development(out, spec, cfg)
  out$score_financial_value <- score_compensation_adjusted_value(out, out$score_contribution_index)

  out$score_model_uncertainty <- score_model_uncertainty(out, out$role_metric_coverage)
  out$score_sporting_volatility <- score_sporting_volatility(out)
  out$score_acquisition_complexity <- score_acquisition_complexity(out)
  out$score_risk <- out$score_model_uncertainty

  out$confidence <- dplyr::case_when(
    out$minutes >= 2000 & (out$tf_uncertainty %||% 0.15) <= 0.12 &
      (out$role_metric_coverage %||% 0) >= 0.85 ~ "high",
    out$minutes >= 900 & (out$role_metric_coverage %||% 0) >= 0.50 ~ "medium",
    TRUE ~ "low"
  )
  out$confidence <- dplyr::case_when(
    is.na(out$role_fit_confidence_cap) ~ "low",
    out$role_fit_confidence_cap == "low" & out$confidence == "high" ~ "low",
    out$role_fit_confidence_cap == "low" & out$confidence == "medium" ~ "low",
    out$role_fit_confidence_cap == "medium" & out$confidence == "high" ~ "medium",
    TRUE ~ out$confidence
  )

  out$data_quality <- clip(
    100 - out$score_model_uncertainty * 0.4 - ifelse(out$confidence == "low", 15, 0),
    0, 100
  )

  out$strengths <- purrr::pmap_chr(
    list(out$score_contribution_index, out$score_role_fit, out$score_financial_value, out$score_development),
    function(p, r, f, d) {
      bits <- c()
      if (is.finite(p) && p >= 65) bits <- c(bits, "Elevated estimated contribution index")
      if (is.finite(r) && r >= 65) bits <- c(bits, "Strong role fit (observed metrics)")
      if (is.finite(f) && f >= 65) bits <- c(bits, "Favorable compensation-adjusted value index")
      if (is.finite(d) && d >= 65) bits <- c(bits, "Development outlook")
      if (!length(bits)) bits <- "Balanced profile without elite standout traits"
      paste(bits, collapse = "; ")
    }
  )

  out$risks_text <- purrr::pmap_chr(
    list(out$score_model_uncertainty, out$score_sporting_volatility,
         out$score_acquisition_complexity, out$confidence, out$tf_uncertainty),
    function(mu, sv, ac, conf, unc) {
      bits <- c()
      if (is.finite(mu) && mu >= 60) bits <- c(bits, "Elevated model uncertainty")
      if (is.finite(sv) && sv >= 60) bits <- c(bits, "Elevated sporting volatility")
      if (is.finite(ac) && ac >= 60) bits <- c(bits, "Elevated acquisition/roster complexity")
      if (identical(conf, "low")) bits <- c(bits, "Small / noisy sample or low metric coverage")
      if (is.finite(unc) && unc >= 0.2) bits <- c(bits, "Large assumed league-translation uncertainty")
      if (!length(bits)) {
        bits <- paste(
          "No major statistical or data-quality flags detected.",
          "Medical, contractual, personal and internal scouting information is unavailable."
        )
      }
      paste(bits, collapse = "; ")
    }
  )

  out$video_questions <- dplyr::case_when(
    role_id == "pressing_striker" ~ "Does he counter-press immediately after loss? First step vs back line? Hold-up vs bounce pressure?",
    role_id == "transition_winger" ~ "Decision speed in first 3s after regain? Weak-foot crossing? Tracks back to compact?",
    role_id == "ball_winning_midfielder" ~ "Angle of engagement? Pass selection under pressure after regain? Discipline on yellow-card risk?",
    role_id == "progressive_center_back" ~ "Line-breaking pass under press? Recovery pace? Aerial command on long diagonals?",
    role_id == "overlapping_fullback" ~ "Timing of overlap vs underlap? Cross selection? 1v1 defending in transition?",
    TRUE ~ "Confirm athletic profile, decision-making, and role-specific cues on video."
  )

  out$reference_percentile_contribution <- percentile_rank(out$score_contribution_index)
  out
}

apply_overall_score <- function(df, weights = NULL, cfg = NULL, apply_risk = TRUE, risk_lambda = 0.15) {
  # Explicit weights (e.g. UI/tests) override model_spec structure.
  # Missing components are excluded and remaining weights are renormalized — never filled with 50.
  if (!is.null(weights)) {
    w <- unlist(weights)
    w[w < 0] <- 0
    comps <- list(
      projected_mls_performance = df$score_projected_mls %||% df$score_contribution_index,
      tactical_role_fit = df$score_role_fit,
      financial_value = df$score_financial_value,
      acquisition_feasibility = df$score_feasibility,
      development_upside = df$score_development
    )
    overall <- vapply(seq_len(nrow(df)), function(i) {
      vals <- vapply(names(comps), function(nm) {
        v <- comps[[nm]]
        if (is.null(v)) return(NA_real_)
        as.numeric(v[[i]])
      }, numeric(1))
      ww <- as.numeric(w[names(comps)])
      names(ww) <- names(comps)
      ok <- is.finite(vals) & is.finite(ww) & ww > 0
      if (!any(ok)) return(NA_real_)
      sum(ww[ok] / sum(ww[ok]) * vals[ok])
    }, numeric(1))
    if (apply_risk) {
      unc <- dplyr::coalesce(df$score_model_uncertainty, df$score_risk)
      overall <- ifelse(is.finite(overall), overall * (1 - risk_lambda * dplyr::coalesce(unc, 0) / 100), NA_real_)
    }
    df$score_overall <- clip(overall, 0, 100)
    return(df)
  }

  spec <- tryCatch(load_model_spec(), error = function(e) NULL)
  if (!is.null(spec$sporting_score) && !is.null(spec$recruitment_priority)) {
    ss <- spec$sporting_score
    sporting <- (ss$contribution %||% 0.55) * dplyr::coalesce(df$score_contribution_index, df$score_projected_mls, 50) +
      (ss$role_fit %||% 0.30) * dplyr::coalesce(df$score_role_fit, 50) +
      (ss$development %||% 0.15) * dplyr::coalesce(df$score_development, 50)

    rp <- spec$recruitment_priority
    overall <- (rp$sporting %||% 0.65) * sporting +
      (rp$compensation_value %||% 0.15) * dplyr::coalesce(df$score_financial_value, 50) +
      ((rp$style %||% 0.15) + (rp$pathway %||% 0.05)) * sporting

    if (apply_risk) {
      overall <- overall * (1 - risk_lambda * df$score_model_uncertainty / 100)
    }
    gate <- spec$feasibility_gate$low_threshold %||% 35
    overall <- ifelse(df$score_feasibility < gate, pmin(overall, 54), overall)

    df$score_sporting <- clip(sporting, 0, 100)
    df$score_overall <- clip(overall, 0, 100)
    return(df)
  }

  weights <- cfg$scoring$weights
  w <- unlist(weights)
  w <- w / sum(w)

  overall <- w[["projected_mls_performance"]] * dplyr::coalesce(df$score_projected_mls, 50) +
    w[["tactical_role_fit"]] * dplyr::coalesce(df$score_role_fit, 50) +
    w[["financial_value"]] * dplyr::coalesce(df$score_financial_value, 50) +
    w[["acquisition_feasibility"]] * dplyr::coalesce(df$score_feasibility, 50) +
    w[["development_upside"]] * dplyr::coalesce(df$score_development, 50)

  if (apply_risk) {
    overall <- overall * (1 - risk_lambda * df$score_risk / 100)
  }
  df$score_overall <- clip(overall, 0, 100)
  df
}

rank_shortlist <- function(df, n = 15) {
  ensure_packages("dplyr")
  df |>
    dplyr::arrange(dplyr::desc(score_overall), score_risk) |>
    dplyr::mutate(
      rank = dplyr::row_number(),
      reference_percentile_overall = percentile_rank(score_overall),
      recommendation = recommendation_label(
        score_overall, score_risk, score_development, score_feasibility
      )
    ) |>
    dplyr::slice_head(n = n)
}

`%||%` <- function(a, b) if (!is.null(a)) a else b
