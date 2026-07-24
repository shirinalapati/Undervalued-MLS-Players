#!/usr/bin/env Rscript
# 05_train_models.R — component scores for each evaluation period

source("R/utilities/load_project.R")
source("R/utilities/model_spec.R")
source("R/utilities/data_provenance.R")
source("R/models/learned/artifact_io.R")
source("R/models/learned/contribution_model.R")
source("R/models/learned/shrinkage_model.R")
source("R/models/learned/age_curves_model.R")
source("R/models/learned/feasibility_model.R")
source("R/models/decision_layer.R")
source("R/features/build_features.R")
source("R/models/component_scores.R")

cfg <- load_config()
ensure_packages(c("readr", "dplyr", "purrr", "jsonlite"))
prov <- read_provenance(cfg)

periods <- c("ytd_2026", "full_2025", "blended")
mvp_roles <- unlist(cfg$mvp_roles)

all_scored <- list()
for (period in periods) {
  feat_path <- file.path(cfg$paths$processed, paste0("player_features_", period, ".csv"))
  if (!file.exists(feat_path)) {
    write_log("Missing features for ", period, " — skip.")
    next
  }
  features <- readr::read_csv(feat_path, show_col_types = FALSE)
  write_log("Scoring period ", period, " (", nrow(features), " players)")

  scored <- purrr::map_dfr(mvp_roles, function(role) {
    compute_component_scores(features, role, cfg) |>
      dplyr::mutate(evaluation_period = period)
  })

  scored <- apply_overall_score(
    scored,
    weights = NULL, # use config/model_spec.yml sporting + recruitment priority
    cfg = cfg,
    apply_risk = isTRUE(cfg$scoring$apply_risk_penalty),
    risk_lambda = cfg$scoring$risk_penalty_weight %||% 0.15
  )

  scored$season_label <- evaluation_period_label(period, cfg)
  scored$data_cutoff_label <- cutoff_label(prov)
  scored$is_synthetic <- isTRUE(prov$is_synthetic)
  scored$stats_are_full_season <- identical(period, "full_2025")
  scored$stats_are_ytd <- !identical(period, "full_2025")

  out_path <- file.path(cfg$paths$processed, paste0("player_component_scores_", period, ".csv"))
  readr::write_csv(scored, out_path)
  all_scored[[period]] <- scored
}

default_period <- cfg$project$evaluation_period_default %||% "blended"
if (!is.null(all_scored[[default_period]])) {
  readr::write_csv(
    all_scored[[default_period]],
    file.path(cfg$paths$processed, "player_component_scores.csv")
  )
}

# Persist default predictions if DB exists (non-fatal if unique constraint hits)
db_path <- cfg$paths$database
if (file.exists(db_path) && !is.null(all_scored[[default_period]])) {
  tryCatch({
    source("R/database/db_utils.R")
    con <- db_connect(cfg)
    on.exit(try(DBI::dbDisconnect(con), silent = TRUE), add = TRUE)
    pred <- all_scored[[default_period]] |>
      dplyr::transmute(
        player_id,
        season_year,
        role_id,
        model_version = paste0("mvp_components_v2_", default_period),
        proj_npxg_p90,
        proj_xa_p90,
        proj_gplus_p90,
        score_projected_mls,
        score_role_fit,
        score_feasibility,
        score_development,
        score_financial_value,
        score_risk,
        confidence,
        data_quality,
        strengths_json = strengths,
        risks_json = risks_text,
        video_questions_json = video_questions
      )
    ver <- paste0("mvp_components_v2_", default_period)
    DBI::dbExecute(con, sprintf("DELETE FROM model_predictions WHERE model_version = '%s'", ver))
    DBI::dbWriteTable(con, "model_predictions", as.data.frame(pred), append = TRUE)
    write_log("Wrote ", nrow(pred), " rows to model_predictions (", ver, ").")
  }, error = function(e) {
    write_log("DB model_predictions write skipped: ", conditionMessage(e),
              " — CSV scores remain authoritative for the app.")
  })
}

write_log("Model/component scoring complete. ", cutoff_label(prov))

# Current MLS rosters from 2026 salary guide + 2026 YTD activity flags
source("R/models/roster_analysis.R")
source("R/models/current_rosters.R")
scores_for_rosters <- all_scored[[default_period]] %||% all_scored[["blended"]] %||% all_scored[["ytd_2026"]]
if (!is.null(scores_for_rosters)) {
  tryCatch(
    write_current_mls_rosters(cfg, scores_df = scores_for_rosters),
    error = function(e) write_log("Roster write skipped: ", conditionMessage(e))
  )
} else {
  write_log("Skipped current roster write — no scored players available.")
}
