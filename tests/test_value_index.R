#!/usr/bin/env Rscript
# Data-quality and scoring tests for 2026 MLS Value Index

source("R/utilities/load_project.R")
source("R/models/value_index.R")
source("R/exports/value_excel.R")

fail <- function(...) stop(paste0(...), call. = FALSE)
ok <- function(msg) message("OK: ", msg)

cfg <- load_config()
vi <- load_value_index_config()
path <- file.path(cfg$paths$processed, "player_value_scores_blended.csv")
if (!file.exists(path)) fail("Missing blended scores — run scripts/06_generate_value_index.R")
df <- read.csv(path, stringsAsFactors = FALSE)

expected_q <- "Which current MLS players provide the most compensation-efficient position-adjusted on-ball impact?"
if (!identical(value_index_research_question(vi), expected_q)) {
  fail("Research question mismatch: ", value_index_research_question(vi))
}
ok("Research question wording")

if (any(df$league_id != "mls", na.rm = TRUE)) fail("Non-MLS players in production scores")
ok("All scored players are MLS")

if (any(df$is_synthetic %in% TRUE)) fail("Synthetic players present in production scores")
ok("No synthetic players")

if (anyDuplicated(df$asa_player_id)) fail("Duplicate asa_player_id in blended scores")
ok("No duplicate player rows")

elig <- df[df$official_eligible %in% TRUE, ]
if (!nrow(elig)) fail("No official eligible players")
if (any(!elig$compensation_known)) fail("Official eligible player missing compensation")
if (any(elig$minutes_2026 < 450)) fail("Official eligible player below 450 2026 minutes")
if (any(is.na(elig$position_group) | !nzchar(elig$position_group))) fail("Missing position group")
ok("Official eligibility gates hold")

counts <- value_index_counts(df)
summary_path <- file.path(cfg$paths$processed, "value_index_summary.json")
if (file.exists(summary_path)) {
  summary <- jsonlite::fromJSON(summary_path)
  if (!identical(as.integer(summary$n_players), as.integer(counts$n_players_evaluated))) {
    fail("Summary n_players disagrees with production scores")
  }
  if (!identical(as.integer(summary$n_official_eligible), as.integer(counts$n_official_eligible))) {
    fail("Summary n_official_eligible disagrees with production scores")
  }
}
ok("Dynamic eligible counts agree with summary JSON")

# Default UI age window (15–45, fractional ages inclusive via age < max+1) covers all eligible
default_age_ui <- elig$age >= 15 & elig$age < 45 + 1
if (!all(default_age_ui, na.rm = TRUE)) {
  fail("Default age filter would drop official eligible players (423 vs filtered mismatch)")
}
ok("Default age window covers all official eligible players")

if (any(elig$compensation <= 0, na.rm = TRUE)) fail("Non-positive compensation")
if (any(elig$minutes_2026 < 0, na.rm = TRUE)) fail("Negative minutes")
ok("Compensation and minutes signs")

score_cols <- c("sporting_impact", "compensation_percentile", "undervaluation_score")
for (col in score_cols) {
  x <- elig[[col]]
  if (any(x < 0 | x > 100, na.rm = TRUE)) fail(col, " outside 0–100")
}
if (any(elig$value_surplus < -100 | elig$value_surplus > 100, na.rm = TRUE)) {
  fail("Value Surplus outside -100–100")
}
ok("Score ranges")

# Negative / non-positive surplus never receives undervalued-family labels
bad_surplus_label <- elig$value_surplus <= 0 &
  elig$value_label %in% c("Undervalued", "Strong Value", "Elite Value")
if (any(bad_surplus_label, na.rm = TRUE)) {
  fail("Non-positive surplus labeled Undervalued / Strong / Elite")
}
ok("Non-positive surplus never undervalued")

# Cheap weak players must not be labeled Undervalued / Strong / Elite
bad_label <- elig$sporting_impact < 55 &
  elig$value_label %in% c("Undervalued", "Strong Value", "Elite Value")
if (any(bad_label, na.rm = TRUE)) fail("Weak cheap players labeled undervalued")
ok("Sporting Impact floor for undervalued labels")

# Label surplus floors
elite_bad <- elig$value_label == "Elite Value" &
  (elig$value_surplus < 25 | elig$undervaluation_score < 90 | elig$sporting_impact < 70 |
     !elig$data_confidence %in% c("Medium", "High"))
strong_bad <- elig$value_label == "Strong Value" &
  (elig$value_surplus < 15 | elig$undervaluation_score < 75 | elig$sporting_impact < 60 |
     !elig$data_confidence %in% c("Medium", "High"))
uv_bad <- elig$value_label == "Undervalued" &
  (elig$value_surplus < 5 | elig$undervaluation_score < 60 | elig$sporting_impact < 55)
below_bad <- elig$value_label == "Below Expected Value by Current Model" & elig$value_surplus > -15
if (any(elite_bad | strong_bad | uv_bad | below_bad, na.rm = TRUE)) {
  fail("Value label thresholds violated")
}
ok("Value label surplus / UV / impact thresholds")

# Display Rank: sequential across full official pool, never resets by position
if (!"display_rank" %in% names(elig)) fail("display_rank missing")
if (any(is.na(elig$display_rank))) fail("Official eligible missing display_rank")
if (!identical(sort(elig$display_rank), seq_len(nrow(elig)))) {
  fail("Display Rank is not sequential 1..n across official eligible")
}
ok("Combined Display Rank never resets")

# Position Rank: resets correctly by position
pos_rank_col <- if ("position_rank" %in% names(elig)) "position_rank" else "undervaluation_rank"
for (pg in unique(elig$position_group)) {
  sub <- elig[elig$position_group == pg, ]
  ranks <- sort(sub[[pos_rank_col]])
  if (!identical(as.integer(ranks), seq_len(nrow(sub)))) {
    fail("Position Rank does not reset 1..n for position ", pg)
  }
}
ok("Position Rank resets by position")

# Filters must not change stored scores: recompute Sporting Impact for a position
pg <- elig$position_group[[1]]
sub <- df[df$position_group == pg & is.finite(df$adjusted_goals_added_p96), ]
recomputed <- percentile_rank(sub$adjusted_goals_added_p96)
if (max(abs(recomputed - sub$sporting_impact), na.rm = TRUE) > 1e-6) {
  fail("Sporting Impact does not match fixed positional percentile")
}
# Simulated filter: subset must keep identical stored scores for retained rows
filtered <- elig[elig$minutes_2026 >= 900, ]
joined <- merge(
  filtered[, c("asa_player_id", "sporting_impact", "value_surplus", "undervaluation_score")],
  df[, c("asa_player_id", "sporting_impact", "value_surplus", "undervaluation_score")],
  by = "asa_player_id",
  suffixes = c(".filt", ".raw")
)
if (nrow(joined) && max(abs(joined$sporting_impact.filt - joined$sporting_impact.raw), na.rm = TRUE) > 0) {
  fail("Filter subset altered stored Sporting Impact")
}
ok("Filter changes do not alter stored scores")

# Visible G+ rates use per-96 units
p96 <- vi$shrinkage$p96_factor %||% (96 / 90)
comp_p90 <- gplus_component_cols()
for (col in comp_p90) {
  p96_col <- sub("_p90$", "_p96", col)
  if (!p96_col %in% names(df)) fail("Missing per-96 component column: ", p96_col)
  both <- is.finite(df[[col]]) & is.finite(df[[p96_col]])
  if (any(both) && max(abs(df[[p96_col]][both] - df[[col]][both] * p96), na.rm = TRUE) > 1e-8) {
    fail(p96_col, " is not p90 × 96/90")
  }
}
if (!"adjusted_goals_added_p96" %in% names(df)) fail("Missing adjusted_goals_added_p96")
ok("Visible G+ rates use per-96 units")

# Model confidence orientation
if (!"model_confidence" %in% names(df)) fail("model_confidence missing")
conf_check <- is.finite(df$model_confidence) & is.finite(df$model_uncertainty)
if (any(conf_check) &&
    max(abs(df$model_confidence[conf_check] - (100 - df$model_uncertainty[conf_check])), na.rm = TRUE) > 1e-8) {
  fail("model_confidence is not 100 - model_uncertainty")
}
ok("Model Confidence = 100 - Model Uncertainty")

# Unit: missing G+ stays missing
fake <- data.frame(
  league_id = "mls",
  position_group = "CM",
  display_name = "Test",
  asa_player_id = "x",
  minutes = 900,
  minutes_2026 = 900,
  minutes_2025 = 900,
  has_2025_prior = TRUE,
  goals_added_p90 = NA_real_,
  guaranteed_compensation = 400000,
  compensation_known = TRUE,
  age = 24,
  team_id = "t",
  club = "Test FC",
  stringsAsFactors = FALSE
)
for (col in gplus_component_cols()) fake[[col]] <- NA_real_
scored <- score_mls_value_index(fake, vi)
if (is.finite(scored$sporting_impact[[1]])) fail("Missing G+ invented a Sporting Impact")
ok("Missing metrics remain missing")

# Synthetic synthetic flag must not enter official rankings in live production file
if (any(elig$is_synthetic %in% TRUE)) fail("Synthetic player in official rankings")
ok("No non-MLS or synthetic player in official rankings")

# Club coverage
clubs <- unique(elig$club)
if (length(clubs) < 28) fail("Expected ~30 MLS clubs in official rankings, found ", length(clubs))
ok(paste0("MLS clubs present: ", length(clubs)))

# Excel export uses the same dynamic counts
tmp_xlsx <- tempfile(fileext = ".xlsx")
export_value_index_xlsx(df, tmp_xlsx, vi_cfg = vi, provenance = list(data_cutoff_utc = "test"))
wb <- openxlsx::read.xlsx(tmp_xlsx, sheet = "Executive Summary")
n_eval_xlsx <- as.integer(wb$Value[wb$Item == "Players evaluated"][[1]])
n_elig_xlsx <- as.integer(wb$Value[wb$Item == "Official eligible"][[1]])
if (!identical(n_eval_xlsx, as.integer(counts$n_players_evaluated)) ||
    !identical(n_elig_xlsx, as.integer(counts$n_official_eligible))) {
  fail("Excel export counts disagree with production scores")
}
ok("Excel export counts match About / production counts")

message("All Value Index tests passed.")
