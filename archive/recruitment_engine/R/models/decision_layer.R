# Decision layer — CONFIGURABLE club utility over model outputs.
# Not a black-box prediction. Clubs override via profiles / dashboard / policy templates.

#' Resolve decision weights: policy template → club profile nudges → explicit overrides.
resolve_decision_weights <- function(club_profile = NULL,
                                     priority = c("balanced", "development", "immediate"),
                                     overrides = NULL,
                                     spec = NULL) {
  priority <- match.arg(priority)
  spec <- spec %||% load_model_spec()

  # Start from model_spec recruitment priority + sporting decomposition
  ss <- spec$sporting_score
  rp <- spec$recruitment_priority

  # Flat utility weights over atomic inputs (sum ≈ 1)
  w <- c(
    contribution = (rp$sporting %||% 0.65) * (ss$contribution %||% 0.55),
    role_fit     = (rp$sporting %||% 0.65) * (ss$role_fit %||% 0.30),
    development  = (rp$sporting %||% 0.65) * (ss$development %||% 0.15),
    style        = rp$style %||% 0.15,
    value        = rp$compensation_value %||% 0.15,
    pathway      = rp$pathway %||% 0.05,
    feasibility  = 0 # gate by default, not a continuous reward
  )

  # Policy templates (configurable examples — not learned truth)
  if (identical(priority, "immediate") && !is.null(spec$policy_templates$win_now)) {
    t <- unlist(spec$policy_templates$win_now)
    w["contribution"] <- t[["contribution"]] %||% w["contribution"]
    w["role_fit"] <- t[["role_fit"]] %||% w["role_fit"]
    w["feasibility"] <- t[["feasibility"]] %||% 0
    w["value"] <- t[["value"]] %||% w["value"]
    w["development"] <- t[["development"]] %||% 0
    w["style"] <- 0.05
    w["pathway"] <- 0.05
  }
  if (identical(priority, "development") && !is.null(spec$policy_templates$development)) {
    t <- unlist(spec$policy_templates$development)
    w["contribution"] <- t[["contribution"]] %||% w["contribution"]
    w["role_fit"] <- t[["role_fit"]] %||% w["role_fit"]
    w["development"] <- t[["development"]] %||% w["development"]
    w["value"] <- t[["value"]] %||% w["value"]
    w["feasibility"] <- t[["feasibility"]] %||% 0
    w["style"] <- 0.05
    w["pathway"] <- 0.05
  }

  # Club profile nudges (still configurable, not learned club utility)
  if (!is.null(club_profile)) {
    fin <- club_profile$financial_value_weight %||% 0.5
    dev <- club_profile$development_priority %||% 0.5
    imm <- club_profile$immediate_impact_priority %||% 0.5
    w["value"] <- w["value"] * (0.6 + 0.8 * fin)
    w["development"] <- w["development"] * (0.55 + 0.9 * dev)
    w["contribution"] <- w["contribution"] * (0.55 + 0.9 * imm)
  }

  if (!is.null(overrides)) {
    for (nm in names(overrides)) {
      if (nm %in% names(w)) w[[nm]] <- as.numeric(overrides[[nm]])
    }
  }

  w[w < 0] <- 0
  if (sum(w) <= 0) w["contribution"] <- 1
  w / sum(w)
}

#' Transparent club utility over hybrid model outputs.
club_utility_score <- function(df, weights, uncertainty_penalty = 0.15,
                               feasibility_gate = 35) {
  getc <- function(primary, fallback = NULL, default = 50) {
    if (primary %in% names(df)) {
      return(dplyr::coalesce(as.numeric(df[[primary]]), default))
    }
    if (!is.null(fallback) && fallback %in% names(df)) {
      return(dplyr::coalesce(as.numeric(df[[fallback]]), default))
    }
    rep(default, nrow(df))
  }

  raw <- (weights[["contribution"]] %||% 0) * getc("score_contribution_index", "score_projected_mls") +
    (weights[["role_fit"]] %||% 0) * getc("score_role_fit") +
    (weights[["development"]] %||% 0) * getc("score_development") +
    (weights[["style"]] %||% 0) * getc("score_club_fit") +
    (weights[["value"]] %||% 0) * getc("score_financial_value") +
    (weights[["pathway"]] %||% 0) * getc("score_pathway_fit") +
    (weights[["feasibility"]] %||% 0) * getc("score_feasibility")

  unc <- getc("score_model_uncertainty", "score_risk")
  overall <- raw * (1 - uncertainty_penalty * unc / 100)
  overall <- ifelse(getc("score_feasibility") < feasibility_gate, pmin(overall, 54), overall)
  clip(overall, 0, 100)
}

#' Apply decision layer; stores weights_used JSON for auditability.
apply_decision_layer <- function(df, club_profile = NULL,
                                 priority = "balanced",
                                 overrides = NULL,
                                 spec = NULL,
                                 uncertainty_penalty = 0.15) {
  ensure_packages("jsonlite")
  spec <- spec %||% load_model_spec()
  w <- resolve_decision_weights(club_profile, priority, overrides, spec)
  gate <- spec$feasibility_gate$low_threshold %||% 35
  df$score_overall <- club_utility_score(df, w, uncertainty_penalty, gate)
  df$decision_weights <- as.character(jsonlite::toJSON(as.list(w), auto_unbox = TRUE))
  df$decision_layer_note <- "Configurable club utility over hybrid model outputs — not a learned universal blend."
  df
}

`%||%` <- function(a, b) if (!is.null(a)) a else b
