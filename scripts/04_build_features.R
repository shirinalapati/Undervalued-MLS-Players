#!/usr/bin/env Rscript
# 04_build_features.R
# Builds features for each evaluation period: ytd_2026, full_2025, blended.

source("R/utilities/load_project.R")
source("R/utilities/data_provenance.R")
source("R/features/build_features.R")
source("R/features/evaluation_periods.R")

cfg <- load_config()
ensure_packages(c("readr", "dplyr"))
prov <- read_provenance(cfg)

multi_path <- file.path(cfg$paths$interim, "player_season_multi.csv")
if (!file.exists(multi_path)) {
  stop("Missing player_season_multi.csv — run scripts/02_clean_data.R first.")
}
multi <- readr::read_csv(multi_path, show_col_types = FALSE)

periods <- c("ytd_2026", "full_2025", "blended")
dir_create_safe(cfg$paths$processed)

# If synthetic/demo multi has only one synthetic season, fabricate lightweight 2025/2024 copies for period plumbing
if (isTRUE(prov$is_synthetic) || !any(multi$season_year == 2025, na.rm = TRUE)) {
  write_log("Synthetic/incomplete multi-season frame — synthesizing period scaffolding for demo.")
  base <- multi
  s2026 <- base |> dplyr::mutate(season_year = 2026L, season_status = "synthetic_ytd")
  s2025 <- base |>
    dplyr::mutate(
      season_year = 2025L,
      season_status = "synthetic_full",
      minutes = minutes * 1.05,
      player_id = paste0(asa_player_id %||% player_id, "_2025")
    )
  s2024 <- base |>
    dplyr::mutate(
      season_year = 2024L,
      season_status = "synthetic_trend",
      goals_added_p90 = goals_added_p90 - dplyr::coalesce(yoy_delta, 0),
      player_id = paste0(asa_player_id %||% player_id, "_2024")
    )
  multi <- dplyr::bind_rows(s2026, s2025, s2024)
  readr::write_csv(multi, multi_path)
}

for (period in periods) {
  write_log("Building features for evaluation period: ", period)
  eval_df <- build_evaluation_table(multi, cfg, period = period)

  # Period-specific minutes floor
  min_m <- if (identical(period, "ytd_2026")) {
    cfg$project$min_minutes_ytd %||% 180
  } else {
    cfg$project$min_minutes_full %||% 450
  }
  cfg_period <- cfg
  cfg_period$project$min_minutes <- min_m

  feats <- build_features(eval_df, cfg_period)
  feats$evaluation_period <- period
  feats$season_label <- evaluation_period_label(period, cfg)
  feats$stats_are_full_season <- identical(period, "full_2025")
  feats$stats_are_ytd <- !identical(period, "full_2025")
  feats$data_cutoff_label <- cutoff_label(prov)
  feats$is_synthetic <- isTRUE(prov$is_synthetic)

  out_path <- file.path(cfg$paths$processed, paste0("player_features_", period, ".csv"))
  readr::write_csv(feats, out_path)
  write_log("Wrote ", nrow(feats), " rows → ", out_path)
}

# Default period features path used by older scripts
default_period <- cfg$project$evaluation_period_default %||% "blended"
file.copy(
  file.path(cfg$paths$processed, paste0("player_features_", default_period, ".csv")),
  file.path(cfg$paths$processed, "player_features.csv"),
  overwrite = TRUE
)
write_log("Default features copy: ", default_period, " | ", cutoff_label(prov))
