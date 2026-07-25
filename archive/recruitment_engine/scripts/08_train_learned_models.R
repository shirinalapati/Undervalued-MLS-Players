#!/usr/bin/env Rscript
# 08_train_learned_models.R — Fit hybrid learned components when data allows.
# Never overwrites configurable club preferences or recommendation invariants.
#
# Time split (from model_spec.hybrid.time_split):
#   Train ≤ 2023 | Validate 2024 | Test 2025
# Live 2026: train only on data available before prediction cutoff.

source("R/utilities/load_project.R")
source("R/utilities/model_spec.R")
source("R/utilities/data_provenance.R")
source("R/models/learned/artifact_io.R")
source("R/models/learned/contribution_model.R")
source("R/models/learned/league_translation_model.R")
source("R/models/learned/age_curves_model.R")
source("R/models/learned/shrinkage_model.R")
source("R/models/learned/cost_model.R")
source("R/models/learned/feasibility_model.R")

cfg <- load_config()
spec <- load_model_spec()
ensure_packages(c("dplyr", "readr", "jsonlite", "tibble"))

multi_path <- file.path(cfg$paths$interim, "player_season_multi.csv")
if (!file.exists(multi_path)) {
  write_log("No player_season_multi.csv — cannot train learned models. Run clean pipeline first.")
  quit(status = 0)
}

players <- readr::read_csv(multi_path, show_col_types = FALSE)
write_log("Loaded ", nrow(players), " player-seasons for hybrid training.")

ensure_artifact_dir(cfg, spec)
report <- list(
  model_version = spec$model_version,
  trained_at = as.character(Sys.time()),
  principle = spec$hybrid$principle,
  components = list()
)

# --- Shrinkage m0 ---
shrink <- estimate_shrinkage_m0(players)
if (!is.null(shrink)) {
  save_learned_artifact(shrink, "shrinkage", cfg, spec)
  report$components$shrinkage <- list(status = "fitted", default_m0 = shrink$default_m0)
  write_log("Saved shrinkage m0 estimates.")
} else {
  report$components$shrinkage <- list(status = "fallback_fixed_m0")
}

# --- Age curves ---
ages <- fit_age_curves(players)
if (!is.null(ages)) {
  save_learned_artifact(ages, "age_curves", cfg, spec)
  report$components$age_curves <- list(status = "fitted", peaks = ages$peaks)
  write_log("Saved learned age curves.")
} else {
  report$components$age_curves <- list(status = "fallback_priors")
}

# --- League translation ---
movers <- identify_league_movers(players)
trans <- fit_league_translation(movers)
if (!is.null(trans)) {
  save_learned_artifact(trans, "league_translation", cfg, spec)
  report$components$league_translation <- list(status = "fitted", n_movers = trans$n)
  write_log("Saved league translation model (n=", trans$n, ").")
} else {
  report$components$league_translation <- list(
    status = "assumed_tier_priors",
    n_movers = nrow(movers),
    note = "Insufficient movers — keep assumed league-strength adjustments."
  )
}

# --- Contribution (needs feature panel; try multi seasons with basic cols) ---
panel <- build_contribution_training_panel(players)
if (nrow(panel)) {
  # Attach crude percentiles within season×position for learning features
  panel <- panel |>
    dplyr::group_by(season_year, position_group) |>
    dplyr::mutate(
      pct_proj_npxg = percentile_rank(npxg_p90),
      pct_proj_xa = percentile_rank(xa_p90),
      pct_proj_gplus = percentile_rank(goals_added_p90),
      pct_proj_press = percentile_rank(pressures_p90),
      pct_prog_pass = percentile_rank(progressive_passes_p90),
      pct_prog_carry = percentile_rank(progressive_carries_p90),
      pct_tackles = percentile_rank(tackles_p90),
      pct_intercept = percentile_rank(interceptions_p90),
      pct_shots = percentile_rank(shots_p90),
      tf_uncertainty = 0.15
    ) |>
    dplyr::ungroup()
  panel <- assign_time_splits(panel, spec)
  # Live rule: for product season training, drop seasons after cutoff if set
  contrib <- fit_contribution_ridge(panel, alpha = 0)
  if (!is.null(contrib)) {
    # Validate / test metrics
    for (sp in c("validate", "test")) {
      sub <- panel[panel$split == sp, , drop = FALSE]
      if (!nrow(sub)) next
      pred <- predict_contribution_model(contrib, sub)
      contrib[[paste0("metrics_", sp)]] <- evaluate_contribution_predictions(sub$outcome_next, pred)
    }
    save_learned_artifact(contrib, "contribution", cfg, spec)
    report$components$contribution <- list(
      status = "fitted",
      model_type = contrib$model_type,
      train_n = contrib$train_n,
      validate = contrib$metrics_validate,
      test = contrib$metrics_test
    )
    write_log("Saved contribution ridge model.")
  } else {
    report$components$contribution <- list(status = "fallback_heuristic")
  }
} else {
  report$components$contribution <- list(status = "fallback_heuristic", note = "No next-season panel")
}

# --- Cost tier ---
cost <- fit_cost_tier_model(players)
if (!is.null(cost)) {
  save_learned_artifact(cost, "cost_tier", cfg, spec)
  report$components$cost_tier <- list(status = "fitted", n = cost$n)
} else {
  report$components$cost_tier <- list(status = "unknown_when_unobserved")
}

# --- Acquisition probability ---
acq_lab <- build_acquisition_labels(players)
acq <- fit_acquisition_probability(acq_lab)
if (!is.null(acq)) {
  save_learned_artifact(acq, "acquisition_probability", cfg, spec)
  report$components$acquisition_probability <- list(
    status = "fitted", n = acq$n, positives = acq$positives
  )
} else {
  report$components$acquisition_probability <- list(status = "fallback_heuristic")
}

# Configurable / rules — never trained here
report$configurable <- spec$hybrid$configurable
report$rules <- spec$hybrid$rules
report$note <- paste(
  "Decision-layer weights and club preferences remain configurable.",
  "Recommendation invariants remain transparent rules.",
  "Do not treat fitted coefficients as club utility."
)

out <- file.path(cfg$paths$processed, "models", "hybrid_training_report.json")
dir_create_safe(dirname(out))
jsonlite::write_json(report, out, pretty = TRUE, auto_unbox = TRUE)
write_log("Hybrid training report: ", out)
message("Learned where possible; fallbacks retained. See docs/architecture.md.")
