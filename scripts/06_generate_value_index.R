#!/usr/bin/env Rscript
# 06_generate_value_index.R
# Build MLS-only Value Index scores for ytd_2026, full_2025, and blended.

source("R/utilities/load_project.R")
source("R/utilities/data_provenance.R")
source("R/models/value_index.R")

cfg <- load_config()
vi_cfg <- load_value_index_config()
ensure_packages(c("readr", "dplyr", "jsonlite"))
prov <- read_provenance(cfg)

if (isTRUE(prov$is_synthetic) && identical(cfg$project$mode, "live")) {
  stop("Refusing to generate production Value Index from synthetic data in live mode.")
}

multi_path <- file.path(cfg$paths$interim, "player_season_multi.csv")
teams_path <- file.path(cfg$paths$interim, "teams_clean.csv")
if (!file.exists(multi_path)) {
  stop("Missing player_season_multi.csv — run scripts/02_clean_data.R first.")
}

multi <- readr::read_csv(multi_path, show_col_types = FALSE)
teams <- if (file.exists(teams_path)) {
  readr::read_csv(teams_path, show_col_types = FALSE)
} else {
  NULL
}

dir_create_safe(cfg$paths$processed)
periods <- c("blended", "ytd_2026", "full_2025")
all_scores <- list()

for (period in periods) {
  write_log("Scoring MLS Value Index for period: ", period)
  eval_df <- build_mls_evaluation(multi, cfg, period = period)
  eval_df <- attach_team_names(eval_df, teams)
  scores <- score_mls_value_index(eval_df, vi_cfg = vi_cfg)
  scores$data_cutoff_label <- cutoff_label(prov)
  scores$is_synthetic <- isTRUE(prov$is_synthetic)
  scores$data_version <- prov$data_cutoff_utc %||% cutoff_label(prov)

  out_path <- file.path(cfg$paths$processed, paste0("player_value_scores_", period, ".csv"))
  readr::write_csv(scores, out_path)
  write_log(
    "Wrote ", nrow(scores), " players (",
    sum(scores$official_eligible, na.rm = TRUE), " official eligible) → ", out_path
  )
  all_scores[[period]] <- scores
}

default_period <- cfg$project$evaluation_period_default %||% "blended"
file.copy(
  file.path(cfg$paths$processed, paste0("player_value_scores_", default_period, ".csv")),
  file.path(cfg$paths$processed, "player_value_scores.csv"),
  overwrite = TRUE
)

default <- all_scores[[default_period]]
counts <- value_index_counts(default)
summary <- list(
  model_version = vi_cfg$model$version %||% "1.1.0",
  evaluation_period = default_period,
  n_players = counts$n_players_evaluated,
  n_official_eligible = counts$n_official_eligible,
  n_missing_compensation = sum(!default$compensation_known, na.rm = TRUE),
  leagues = unique(as.character(default$league_id)),
  position_groups = as.list(table(default$position_group)),
  clubs = length(unique(default$club)),
  research_question = value_index_research_question(vi_cfg),
  data_cutoff = cutoff_label(prov),
  generated_at = format(Sys.time(), tz = "UTC", usetz = TRUE)
)
jsonlite::write_json(
  summary,
  file.path(cfg$paths$processed, "value_index_summary.json"),
  pretty = TRUE,
  auto_unbox = TRUE
)
write_log("Value Index generation complete | ", cutoff_label(prov))
