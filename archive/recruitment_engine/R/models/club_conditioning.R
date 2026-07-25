# Club-conditioned ranking: personalize shortlists by style, budget, age, and roster prefs.

budget_tier_to_max_cost <- function(budget_tier) {
  dplyr::case_when(
    budget_tier %in% c("low", "low_medium") ~ 2L,
    budget_tier %in% c("medium") ~ 3L,
    budget_tier %in% c("medium_high", "high") ~ 4L,
    budget_tier %in% c("very_high") ~ 5L,
    TRUE ~ 3L
  )
}

suggested_role_for_club <- function(club_profile) {
  arch <- tolower(club_profile$tactical_archetype %||% "")
  dplyr::case_when(
    grepl("press|transition", arch) && grepl("press", arch) ~ "pressing_striker",
    grepl("possession", arch) ~ "ball_winning_midfielder",
    grepl("defensive|organized", arch) ~ "progressive_center_back",
    grepl("transition", arch) ~ "transition_winger",
    TRUE ~ "ball_winning_midfielder"
  )
}

suggested_priority_for_club <- function(club_profile) {
  dev <- club_profile$development_priority %||% 0.5
  imm <- club_profile$immediate_impact_priority %||% 0.5
  if (dev >= 0.65 && dev >= imm) return("development")
  if (imm >= 0.65 && imm > dev) return("immediate")
  "balanced"
}

club_weight_vector <- function(club_profile, base_weights, priority = "balanced") {
  w <- unlist(base_weights)
  # Pull weights toward club recruitment identity
  fin <- club_profile$financial_value_weight %||% 0.5
  dev <- club_profile$development_priority %||% 0.5
  imm <- club_profile$immediate_impact_priority %||% 0.5

  w[["financial_value"]] <- w[["financial_value"]] * (0.6 + 0.8 * fin)
  w[["development_upside"]] <- w[["development_upside"]] * (0.55 + 0.9 * dev)
  w[["projected_mls_performance"]] <- w[["projected_mls_performance"]] * (0.55 + 0.9 * imm)
  w[["acquisition_feasibility"]] <- w[["acquisition_feasibility"]] * (0.7 + 0.6 * fin)

  if (identical(priority, "development")) {
    w[["development_upside"]] <- w[["development_upside"]] + 0.22
    w[["acquisition_feasibility"]] <- w[["acquisition_feasibility"]] + 0.06
    w[["projected_mls_performance"]] <- w[["projected_mls_performance"]] - 0.12
    w[["financial_value"]] <- w[["financial_value"]] + 0.04
  }
  if (identical(priority, "immediate")) {
    w[["projected_mls_performance"]] <- w[["projected_mls_performance"]] + 0.22
    w[["tactical_role_fit"]] <- w[["tactical_role_fit"]] + 0.06
    w[["development_upside"]] <- w[["development_upside"]] - 0.16
    w[["acquisition_feasibility"]] <- w[["acquisition_feasibility"]] - 0.04
  }

  w[w < 0.03] <- 0.03
  w / sum(w)
}

style_club_fit <- function(df, club_profile) {
  # Emphasize traits the club cares about; skip missing metrics (no neutral-50 fill).
  press <- df$pct_proj_press
  poss <- df$pct_pass
  # transition_involvement removed as synthetic; use carry+press blend when both exist
  trans <- ifelse(
    is.finite(df$pct_prog_carry) & is.finite(df$pct_proj_press),
    0.5 * df$pct_prog_carry + 0.5 * df$pct_proj_press,
    NA_real_
  )
  prog <- df$pct_prog_pass
  defn <- df$defensive_actions_p90_pct

  pw <- club_profile$pressing_weight %||% 0.5
  ow <- club_profile$possession_weight %||% 0.5
  tw <- club_profile$transition_weight %||% 0.5
  gw <- club_profile$progression_weight %||% 0.5
  dw <- club_profile$defensive_weight %||% 0.5

  fit <- vapply(seq_len(nrow(df)), function(i) {
    vals <- c(press[[i]], poss[[i]], trans[[i]], prog[[i]], defn[[i]])
    wts <- c(pw, ow, tw, gw, dw)
    ok <- is.finite(vals)
    if (!any(ok)) return(NA_real_)
    sum(wts[ok] * vals[ok]) / sum(wts[ok])
  }, numeric(1))

  if (pw >= max(ow, tw, gw, dw)) {
    fit <- ifelse(is.finite(press), fit * (0.75 + 0.25 * (press / 100)) - pmax(0, 55 - press) * 0.35, fit)
  } else if (ow >= max(pw, tw, gw, dw)) {
    fit <- ifelse(is.finite(poss), fit * (0.75 + 0.25 * (poss / 100)) - pmax(0, 55 - poss) * 0.30, fit)
  } else if (tw >= max(pw, ow, gw, dw)) {
    fit <- ifelse(is.finite(trans), fit * (0.75 + 0.25 * (trans / 100)) - pmax(0, 55 - trans) * 0.30, fit)
  } else if (dw >= max(pw, ow, tw, gw)) {
    fit <- ifelse(is.finite(defn), fit * (0.75 + 0.25 * (defn / 100)) - pmax(0, 55 - defn) * 0.30, fit)
  }

  clip(fit, 0, 100)
}

age_alignment_score <- function(age, club_profile, priority) {
  target <- club_profile$average_squad_age %||% 26.5
  dev <- club_profile$development_priority %||% 0.5
  imm <- club_profile$immediate_impact_priority %||% 0.5

  # Selected Priority drives age fit (club prefs only nudge balanced)
  if (identical(priority, "development")) {
    # Strong preference for teenagers / early-20s
    return(clip(100 - pmax(0, age - 21) * 11, 10, 100))
  }
  if (identical(priority, "immediate")) {
    # Prefer proven peak ages; soft-penalize very young
    return(clip(100 - abs(age - 27) * 9 - pmax(0, 22 - age) * 6, 15, 100))
  }
  if (dev >= 0.65) {
    return(clip(100 - pmax(0, age - 23) * 7, 20, 100))
  }
  if (imm >= 0.65) {
    return(clip(100 - abs(age - 27) * 6, 20, 100))
  }
  clip(100 - abs(age - target) * 6, 25, 100)
}

budget_alignment_score <- function(cost_tier, club_profile) {
  max_cost <- budget_tier_to_max_cost(club_profile$budget_tier %||% "medium")
  dplyr::case_when(
    !is.finite(cost_tier) ~ 50, # unknown compensation — neutral, not fabricated fit
    cost_tier <= max_cost - 1 ~ 95,
    cost_tier == max_cost ~ 78,
    cost_tier == max_cost + 1 ~ 40,
    TRUE ~ 15
  )
}

domestic_alignment_score <- function(is_domestic, club_profile, intl_roster_status = NULL) {
  pref <- club_profile$domestic_player_priority %||% 0.5
  status <- intl_roster_status
  dplyr::case_when(
    !is.null(status) & tolower(as.character(status)) %in% c("domestic", "homegrown") ~
      50 + 45 * pref,
    !is.null(status) & tolower(as.character(status)) == "international" ~
      50 + 45 * (1 - pref),
    TRUE ~ 50 # unknown — do not infer from nationality
  )
}

league_pathway_score <- function(league_id, club_profile) {
  dev <- club_profile$development_priority %||% 0.5
  fin <- club_profile$financial_value_weight %||% 0.5
  imm <- club_profile$immediate_impact_priority %||% 0.5

  dplyr::case_when(
    league_id == "mls" ~ 55 + 35 * imm,
    league_id == "uslc" ~ 50 + 35 * fin + 15 * dev,
    league_id == "mlsnp" ~ 45 + 45 * dev,
    TRUE ~ 50
  )
}

club_conditioned_score <- function(df, club_profile, base_weights,
                                   priority = c("balanced", "development", "immediate"),
                                   club_blend = 0.55,
                                   apply_soft_filters = TRUE) {
  priority <- match.arg(priority)
  ensure_packages(c("dplyr", "jsonlite"))

  w <- club_weight_vector(club_profile, base_weights, priority)

  df$score_club_fit <- style_club_fit(df, club_profile)
  df$score_age_fit <- age_alignment_score(df$age, club_profile, priority)
  df$score_budget_fit <- budget_alignment_score(df$cost_tier, club_profile)
  intl <- if ("intl_roster_status" %in% names(df)) df$intl_roster_status else rep(NA_character_, nrow(df))
  df$score_domestic_fit <- domestic_alignment_score(df$is_domestic_player, club_profile, intl)
  df$score_pathway_fit <- league_pathway_score(df$league_id, club_profile)

  # Soft exclusions — unknown cost tiers are kept (not dropped as over-budget)
  if (isTRUE(apply_soft_filters)) {
    max_cost <- budget_tier_to_max_cost(club_profile$budget_tier %||% "medium")
    keep <- is.na(df$cost_tier) | df$cost_tier <= (max_cost + 1L)
    if (identical(priority, "development")) {
      keep <- keep & df$age <= 24
    } else if (identical(priority, "immediate")) {
      keep <- keep & df$age >= 22 & df$minutes >= 1200
    }
    if (sum(keep, na.rm = TRUE) >= 8) df <- df[keep, , drop = FALSE]
  }

  # Decision layer prefers configurable club utility (see R/models/decision_layer.R)
  spec <- tryCatch(load_model_spec(), error = function(e) NULL)
  if (exists("apply_decision_layer", mode = "function")) {
    df <- apply_decision_layer(
      df,
      club_profile = club_profile,
      priority = priority,
      spec = spec,
      uncertainty_penalty = if (isTRUE(TRUE)) 0.15 else 0
    )
    df$score_sporting <- clip(
      0.55 * dplyr::coalesce(df$score_contribution_index, df$score_projected_mls, 50) +
        0.30 * dplyr::coalesce(df$score_role_fit, 50) +
        0.15 * dplyr::coalesce(df$score_development, 50),
      0, 100
    )
    df$score_club_personalization <- clip(
      0.70 * df$score_club_fit +
        0.20 * df$score_pathway_fit +
        0.10 * df$score_budget_fit,
      0, 100
    )
  } else if (!is.null(spec$sporting_score)) {
    ss <- spec$sporting_score
    sporting <- (ss$contribution %||% 0.55) * dplyr::coalesce(df$score_contribution_index, df$score_projected_mls, 50) +
      (ss$role_fit %||% 0.30) * dplyr::coalesce(df$score_role_fit, 50) +
      (ss$development %||% 0.15) * dplyr::coalesce(df$score_development, 50)
    rp <- spec$recruitment_priority
    overall <- (rp$sporting %||% 0.65) * sporting +
      (rp$style %||% 0.15) * df$score_club_fit +
      (rp$compensation_value %||% 0.15) * dplyr::coalesce(df$score_financial_value, 50) +
      (rp$pathway %||% 0.05) * df$score_pathway_fit
    gate <- spec$feasibility_gate$low_threshold %||% 35
    overall <- ifelse(df$score_feasibility < gate, pmin(overall, 54), overall)
    df$score_sporting <- clip(sporting, 0, 100)
    df$score_overall <- clip(overall, 0, 100)
  } else {
    base_overall <- w[["projected_mls_performance"]] * df$score_projected_mls +
      w[["tactical_role_fit"]] * df$score_role_fit +
      w[["financial_value"]] * dplyr::coalesce(df$score_financial_value, 50) +
      w[["acquisition_feasibility"]] * df$score_feasibility +
      w[["development_upside"]] * df$score_development

    df$score_overall <- clip(
      (1 - club_blend) * base_overall + club_blend * df$score_club_personalization,
      0, 100
    )
  }

  # Deterministic near-tie break that differs by club (does not invent talent)
  club_seed <- sum(utf8ToInt(club_profile$club_id %||% "x"))
  tiebreak <- ((as.numeric(as.factor(df$player_id)) * club_seed) %% 97) / 97 * 1.5
  df$score_overall <- clip(df$score_overall + tiebreak, 0, 100)

  # Fixed-reference percentile within this role-scored cohort (before UI filters re-rank)
  df$reference_percentile_overall <- percentile_rank(df$score_overall)

  df$weights_used <- df$decision_weights %||% as.character(jsonlite::toJSON(as.list(w), auto_unbox = TRUE))
  df$explanation <- sprintf(
    "%s | style=%s | budget=%s | age-target≈%.1f | decision layer = configurable club utility (not learned). press=%.2f poss=%.2f transition=%.2f.",
    club_profile$club_name %||% club_profile$club_id,
    club_profile$tactical_archetype %||% "n/a",
    club_profile$budget_tier %||% "n/a",
    club_profile$average_squad_age %||% NA_real_,
    club_profile$pressing_weight %||% NA_real_,
    club_profile$possession_weight %||% NA_real_,
    club_profile$transition_weight %||% NA_real_
  )
  df$why_club <- sprintf(
    "Style %.0f · Pathway %.0f · BudgetFit %.0f (age used as filter/overlay only)",
    df$score_club_fit, df$score_pathway_fit, df$score_budget_fit
  )
  df
}

`%||%` <- function(a, b) if (!is.null(a)) a else b
