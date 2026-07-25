#!/usr/bin/env Rscript
# Historical backtesting + hybrid status report.
# Until tests pass, product language stays cautious (model_spec recommendation_labels).

source("R/utilities/load_project.R")
source("R/utilities/model_spec.R")
source("R/utilities/data_provenance.R")
source("R/models/learned/artifact_io.R")
source("R/models/learned/contribution_model.R")
source("R/models/roster_comparison.R")

cfg <- load_config()
spec <- load_model_spec()
ensure_packages(c("jsonlite", "dplyr"))

out_dir <- dir_create_safe(file.path(PROJECT_ROOT, "docs", "validation"))

status <- hybrid_status_summary(spec, cfg)

summary <- list(
  model_version = spec$model_version,
  hybrid_principle = spec$hybrid$principle,
  generated_at = as.character(Sys.time()),
  learned_component_status = status,
  time_split = spec$hybrid$time_split,
  required_evaluations = list(
    near_term_contribution = list(
      description = "Cutoff-date features → next-season / next-12m MLS contribution",
      metrics = c("MAE", "RMSE", "rank_correlation", "calibration_by_position"),
      baselines = c("raw_gplus_p90", "xg_xa_p90", "heuristic_role_blend"),
      status = "pending_or_see_hybrid_training_report"
    ),
    league_translation = list(
      description = "USLC/MLSNP→MLS movers; pre/post with CIs",
      status = "pending_until_mover_sample"
    ),
    shortlist_success = list(
      description = "Historical snapshot top-k vs later useful MLS contributors",
      baselines = c("gplus_p90", "xg_xa_p90", "age", "salary", "minutes"),
      status = "pending"
    ),
    stability = list(
      description = "Bootstrap minutes; top-10 retention; recommendation flip rate",
      status = "pending"
    ),
    decision_layer_sensitivity = list(
      description = "Configurable weights change rank, not Model-1 contribution scores",
      status = "required"
    )
  ),
  coefficient_taxonomy = list(
    learned = names(spec$hybrid$learned),
    configurable = spec$hybrid$configurable,
    rules = spec$hybrid$rules
  ),
  language_policy = list(
    allowed = unname(unlist(spec$recommendation_labels)),
    contribution_label = spec$contribution_label,
    value_label = spec$value_label,
    forbidden_until_validated = c(
      "Projected MLS performance",
      "Pursue",
      "Financial surplus",
      "Highly attainable"
    )
  ),
  note = paste(
    "Learn outcomes; configure club utility; enforce recommendation rules.",
    "See docs/architecture.md and docs/ACCURACY_AUDIT.md."
  )
)

path <- file.path(out_dir, "backtest_summary.json")
jsonlite::write_json(summary, path, pretty = TRUE, auto_unbox = TRUE)
write_log("Wrote backtest / hybrid status to ", path)

# Quick invariant smoke (does not require historical archive)
inc <- data.frame(
  display_name = "A", asa_player_id = "a", score_projected_mls = 60, score_role_fit = 60,
  score_development = 50, score_feasibility = 70, score_financial_value = 50, score_risk = 40,
  confidence = "medium", age = 28, salary = 400000, cost_tier = 3L, compensation_known = TRUE,
  league_id = "mls", minutes = 1500, minutes_share = 0.5, score_pathway_fit = 60,
  npxg_p90_pct = 60, xa_p90_pct = 55, pressures_p90_pct = 50, shots_p90_pct = 58,
  defensive_actions_p90_pct = 45, tackles_p90_pct = 40, interceptions_p90_pct = 42,
  progressive_passes_p90_pct = 50, progressive_carries_p90_pct = 48, ball_retention_pct = 50,
  progressive_passes_received_p90_pct = 50, carries_into_final_third_p90_pct = 48,
  successful_dribbles_p90_pct = 48, pass_completion_under_pressure_pct = 50,
  pass_completion_pct_pct = 50, turnovers_forced_p90_pct = NA_real_,
  progressive_runs_p90_pct = NA_real_, transition_involvement_pct = NA_real_,
  fouls_won_ratio_pct = NA_real_, aerial_win_pct_pct = NA_real_, crosses_p90_pct = NA_real_,
  long_pass_completion_pct_pct = NA_real_, touches_att_third_p90_pct = NA_real_,
  video_questions = "n/a", stringsAsFactors = FALSE
)
tgt <- inc
tgt$display_name <- "B"
tgt$asa_player_id <- "b"
tgt$salary <- 900000
tgt$cost_tier <- 4L
tgt$score_projected_mls <- 62
cmp <- compare_incumbent_target(inc, tgt, "pressing_striker", objective = "lower_cost")
stopifnot(identical(cmp$relationship, "No clear recruitment advantage identified"))
write_log("Invariant smoke passed: expensive target is not lower-cost.")

message("Backtest scaffold + hybrid status updated. Run scripts/08_train_learned_models.R to fit artifacts.")
