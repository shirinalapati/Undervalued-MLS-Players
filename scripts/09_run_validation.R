#!/usr/bin/env Rscript
# Historical stability validation for MLS Value Index (descriptive, not a forecast claim)

source("R/utilities/load_project.R")
source("R/models/value_index.R")

cfg <- load_config()
vi <- load_value_index_config()
ensure_packages(c("dplyr", "jsonlite", "readr"))

multi <- readr::read_csv(file.path(cfg$paths$interim, "player_season_multi.csv"), show_col_types = FALSE)
teams <- readr::read_csv(file.path(cfg$paths$interim, "teams_clean.csv"), show_col_types = FALSE)

# Pseudo-historical: treat 2025 full-season as "prior ranking season" and relate to 2026 YTD outcomes
s2025 <- build_mls_evaluation(multi, cfg, "full_2025") |> attach_team_names(teams) |> score_mls_value_index(vi)
s2026 <- build_mls_evaluation(multi, cfg, "ytd_2026") |> attach_team_names(teams) |> score_mls_value_index(vi)

joined <- dplyr::inner_join(
  s2025 |> dplyr::select(
    asa_player_id, position_group,
    impact_2025 = sporting_impact,
    surplus_2025 = value_surplus,
    uv_2025 = undervaluation_score,
    minutes_2025 = minutes,
    eligible_2025 = official_eligible
  ),
  s2026 |> dplyr::select(
    asa_player_id,
    impact_2026 = sporting_impact,
    surplus_2026 = value_surplus,
    uv_2026 = undervaluation_score,
    minutes_2026,
    eligible_2026 = official_eligible
  ),
  by = "asa_player_id"
) |>
  dplyr::filter(eligible_2025, is.finite(impact_2025), is.finite(impact_2026))

corr_impact <- suppressWarnings(cor(joined$impact_2025, joined$impact_2026, use = "complete.obs", method = "spearman"))
corr_surplus <- suppressWarnings(cor(joined$surplus_2025, joined$surplus_2026, use = "complete.obs", method = "spearman"))

top25 <- joined |>
  dplyr::group_by(position_group) |>
  dplyr::mutate(rank_2025 = dplyr::min_rank(dplyr::desc(uv_2025))) |>
  dplyr::ungroup() |>
  dplyr::filter(rank_2025 <= 25)

top25_stability <- mean(top25$impact_2026 >= 50, na.rm = TRUE)

# Baselines vs next-season impact
baseline_gplus <- suppressWarnings(cor(joined$impact_2025, joined$impact_2026, use = "complete.obs"))
# compensation-only: lower 2025 pay standing → higher 2026 impact? (weak baseline)
# Use inverse compensation percentile from 2025 if available — approximate with -surplus residual
baseline_pay <- suppressWarnings(cor(-joined$surplus_2025 + joined$impact_2025, joined$impact_2026, use = "complete.obs"))

report <- list(
  validation_type = "year_to_year_stability",
  claim = "Descriptive stability check — not a guarantee of future breakouts",
  n_players = nrow(joined),
  spearman_sporting_impact_2025_vs_2026 = corr_impact,
  spearman_value_surplus_2025_vs_2026 = corr_surplus,
  top25_share_with_2026_impact_ge_50 = top25_stability,
  baselines = list(
    prior_sporting_impact = baseline_gplus,
    prior_compensation_standing_proxy = baseline_pay
  ),
  by_position = joined |>
    dplyr::group_by(position_group) |>
    dplyr::summarise(
      n = dplyr::n(),
      spearman_impact = suppressWarnings(cor(impact_2025, impact_2026, method = "spearman")),
      .groups = "drop"
    ) |>
    as.data.frame(),
  generated_at = format(Sys.time(), tz = "UTC", usetz = TRUE)
)

dir_create_safe(file.path(PROJECT_ROOT, "docs", "validation"))
dir_create_safe(file.path(PROJECT_ROOT, "reports", "_output"))
jsonlite::write_json(
  report,
  file.path(PROJECT_ROOT, "docs", "validation", "value_index_stability.json"),
  pretty = TRUE,
  auto_unbox = TRUE,
  dataframe = "rows"
)

html <- sprintf(
  "<html><head><title>MLS Value Index Validation</title></head><body>
  <h1>2026 MLS Value Index — Historical Stability</h1>
  <p>Descriptive check relating 2025 full-season scores to 2026 YTD outcomes.</p>
  <ul>
    <li>Players matched: %s</li>
    <li>Spearman Sporting Impact 2025→2026: %.3f</li>
    <li>Spearman Value Surplus 2025→2026: %.3f</li>
    <li>Share of 2025 position top-25 with 2026 Impact ≥ 50: %.1f%%</li>
  </ul>
  <p><em>Do not claim breakout prediction unless stronger outcome validation is published.</em></p>
  </body></html>",
  report$n_players, corr_impact, corr_surplus, 100 * top25_stability
)
writeLines(html, file.path(PROJECT_ROOT, "reports", "_output", "model_validation.html"))
write_log("Validation written to docs/validation/ and reports/_output/model_validation.html")
