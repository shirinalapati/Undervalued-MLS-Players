# Unified configuration loader — Phase 1 source of truth.
# Prefer config/model.yml; keep model_spec.yml as compatibility mirror.

load_model_spec <- function(path = NULL) {
  ensure_packages("yaml")
  candidates <- c(
    path,
    file.path(PROJECT_ROOT, "config", "model.yml"),
    file.path(PROJECT_ROOT, "config", "model_spec.yml")
  )
  candidates <- Filter(function(p) !is.null(p) && file.exists(p), candidates)
  if (!length(candidates)) stop("No model.yml / model_spec.yml found")
  yaml::read_yaml(candidates[[1]])
}

load_roles_config <- function() {
  ensure_packages("yaml")
  path <- file.path(PROJECT_ROOT, "config", "roles.yml")
  if (!file.exists(path)) path <- file.path(PROJECT_ROOT, "config", "role_weights.yml")
  yaml::read_yaml(path)
}

load_thresholds <- function() {
  ensure_packages("yaml")
  path <- file.path(PROJECT_ROOT, "config", "thresholds.yml")
  if (!file.exists(path)) return(list())
  yaml::read_yaml(path)
}

load_recommendation_rules <- function() {
  ensure_packages("yaml")
  path <- file.path(PROJECT_ROOT, "config", "recommendation_rules.yml")
  if (!file.exists(path)) return(list())
  yaml::read_yaml(path)
}

load_data_sources_config <- function() {
  ensure_packages("yaml")
  path <- file.path(PROJECT_ROOT, "config", "data_sources.yml")
  if (!file.exists(path)) return(list())
  yaml::read_yaml(path)
}

load_export_settings <- function() {
  ensure_packages("yaml")
  path <- file.path(PROJECT_ROOT, "config", "export_settings.yml")
  if (!file.exists(path)) return(list())
  yaml::read_yaml(path)
}

#' Merge thresholds + recommendation_rules into the active model spec view.
load_product_config <- function() {
  spec <- load_model_spec()
  thr <- load_thresholds()
  rules <- load_recommendation_rules()
  sources <- load_data_sources_config()
  exports <- load_export_settings()

  if (length(thr$coverage)) spec$coverage <- thr$coverage
  if (length(thr$feasibility_gate)) spec$feasibility_gate <- thr$feasibility_gate
  if (!is.null(thr$forecast$primary_horizon)) {
    spec$forecast_horizon <- thr$forecast$primary_horizon
    spec$forecast_horizon_label <- thr$forecast$primary_horizon_label %||% spec$forecast_horizon
  }
  if (!is.null(thr$forecast$contribution_output_name)) {
    spec$contribution_label <- thr$forecast$contribution_output_name
  }
  if (length(rules$invariants)) spec$invariants <- rules$invariants
  if (length(rules$labels)) spec$recommendation_labels <- rules$labels
  if (!is.null(rules$relationship_fallback)) {
    spec$relationship_fallback <- rules$relationship_fallback
  }
  if (!is.null(sources$data_version)) spec$data_version <- sources$data_version
  spec$thresholds <- thr
  spec$recommendation_rules <- rules
  spec$data_sources <- sources
  spec$export_settings <- exports
  spec
}

#' Concise methodology bullets for About / Excel (no hard-coded competing formulas).
render_methodology_summary <- function(spec = NULL) {
  spec <- spec %||% load_product_config()
  c(
    paste0("Model version: ", spec$model_version %||% "unknown",
           " · data version: ", spec$data_version %||% "unknown"),
    paste0("Status: ", spec$status %||% "unknown"),
    paste0("Forecast horizon: ", spec$forecast_horizon_label %||% spec$forecast_horizon %||% "undefined"),
    paste0("Contribution: ", spec$contribution_label %||% "Estimated Near-Term Contribution",
           " (learned ridge when fitted; else role-specific heuristic — no salary/feasibility inside)"),
    paste0("Value: ", spec$value_label %||% "Compensation-Adjusted Value Index",
           " (not acquisition surplus)"),
    paste0("League translation: ", spec$league_tier_label %||% "Assumed league-strength adjustments"),
    paste0("Decision layer: configurable club utility (sporting ",
           paste(names(spec$sporting_score %||% list()), collapse = "/"),
           ") — not a learned universal blend"),
    paste0("Missing metrics: NA / Not available (never displayed as 50); coverage gates from thresholds.yml"),
    paste0("Recommendations: cautious labels from recommendation_rules.yml; invariants enforced")
  )
}

contribution_weights_for_role <- function(role_id, spec = NULL) {
  spec <- spec %||% load_model_spec()
  roles <- spec$contribution_by_role
  key <- dplyr::case_when(
    role_id %in% c("pressing_striker", "pressing_forward") ~ "pressing_forward",
    role_id %in% c("target_forward", "hold_up_striker") ~ "target_forward",
    role_id %in% c("creative_10", "number_10") ~ "creative_10",
    role_id %in% c("box_to_box_8", "box_to_box") ~ "box_to_box_8",
    role_id %in% c("holding_6", "holding_midfielder") ~ "holding_6",
    role_id %in% c("progressive_center_back", "progressive_cb") ~ "progressive_cb",
    role_id %in% c("ball_winning_midfielder") ~ "ball_winning_midfielder",
    role_id %in% c("transition_winger") ~ "transition_winger",
    role_id %in% c("inverted_winger") ~ "inverted_winger",
    role_id %in% c("overlapping_fullback", "attacking_fullback") ~ "attacking_fullback",
    role_id %in% c("defensive_fullback") ~ "defensive_fullback",
    TRUE ~ "default"
  )
  w <- roles[[key]] %||% roles$default
  unlist(w)
}

age_peak_for_position <- function(position_group, spec = NULL) {
  spec <- spec %||% load_model_spec()
  curves <- spec$age_curves %||% list(default = 26.5)
  pg <- toupper(as.character(position_group))
  key <- dplyr::case_when(
    pg %in% c("FW", "F", "ST", "W") ~ "F",
    pg %in% c("W", "AM", "W_AM") ~ "W_AM",
    pg %in% c("CM", "M", "DM", "AM") ~ "CM",
    pg %in% c("FB", "WB") ~ "FB",
    pg %in% c("CB", "D") ~ "CB",
    pg %in% c("GK") ~ "GK",
    TRUE ~ "default"
  )
  # Map FW/W to F/W_AM when present
  key <- dplyr::case_when(
    key == "F" & pg %in% c("W") ~ "W_AM",
    TRUE ~ key
  )
  vapply(key, function(k) as.numeric(curves[[k]] %||% curves$default %||% 26.5), numeric(1))
}

continuous_age_development <- function(age, position_group, spec = NULL) {
  peak <- age_peak_for_position(position_group, spec)
  age <- as.numeric(age)
  score <- 100 * exp(-0.5 * ((age - (peak - 3)) / 4.5)^2)
  above <- pmax(0, age - peak)
  score <- score - pmin(45, above * 6)
  clip(score, 5, 95)
}

confidence_rank <- function(conf) {
  dplyr::case_when(
    tolower(as.character(conf)) == "high" ~ 3L,
    tolower(as.character(conf)) == "medium" ~ 2L,
    tolower(as.character(conf)) == "low" ~ 1L,
    TRUE ~ 0L
  )
}

min_confidence_met <- function(conf, required = "medium") {
  confidence_rank(conf) >= confidence_rank(required)
}

coverage_max_confidence <- function(coverage, spec = NULL) {
  spec <- spec %||% load_product_config()
  c <- spec$coverage
  dplyr::case_when(
    !is.finite(coverage) | coverage < (c$role_fit_unavailable_below %||% 0.5) ~ NA_character_,
    coverage < (c$max_confidence_low_below %||% 0.7) ~ "low",
    coverage < (c$max_confidence_medium_below %||% 0.85) ~ "medium",
    TRUE ~ "high"
  )
}

fmt_na_display <- function(x, digits = 1) {
  if (length(x) != 1) {
    return(vapply(x, function(v) fmt_na_display(v, digits), character(1)))
  }
  if (!is.finite(as.numeric(x))) return("Not available")
  formatC(as.numeric(x), format = "f", digits = digits)
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
           provenance$transactions_through %||% "not systematically ingested"),
    paste0("Model training data through: ",
           provenance$model_training_through %||% "not fitted / heuristic fallback")
  )
}

`%||%` <- function(a, b) if (!is.null(a)) a else b
