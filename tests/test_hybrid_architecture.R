# Hybrid architecture tests — learned vs configurable vs rules
# Run: Rscript -e 'testthat::test_file("tests/test_hybrid_architecture.R")'

library(testthat)

root <- if (basename(getwd()) == "tests") dirname(getwd()) else getwd()
setwd(root)

source("R/utilities/load_project.R")
source("R/utilities/model_spec.R")
source("R/models/learned/artifact_io.R")
source("R/models/learned/contribution_model.R")
source("R/models/decision_layer.R")
source("R/models/component_scores.R")

spec <- load_model_spec()

test_that("model_spec declares hybrid taxonomy", {
  expect_true(!is.null(spec$hybrid$learned$contribution))
  expect_true("club_preferences" %in% names(spec$hybrid$configurable) ||
                !is.null(spec$hybrid$configurable$club_preferences))
  expect_true("recommendation_invariants" %in% unlist(spec$hybrid$rules))
})

test_that("contribution falls back to heuristic without artifact", {
  df <- data.frame(
    pct_proj_npxg = 70, pct_proj_xa = 60, pct_proj_press = 55,
    pct_proj_gplus = 65, minutes = 1800,
    stringsAsFactors = FALSE
  )
  # Ensure no accidental artifact
  skip_if(!is.null(load_learned_artifact("contribution")), "fitted contribution artifact present")
  out <- score_contribution_hybrid(df, "pressing_striker", spec)
  expect_equal(out$source, "heuristic_fallback")
  expect_true(is.finite(out$score[[1]]))
})

test_that("decision weights differ for win-now vs development policies", {
  w_imm <- resolve_decision_weights(priority = "immediate", spec = spec)
  w_dev <- resolve_decision_weights(priority = "development", spec = spec)
  expect_gt(w_imm[["contribution"]], w_dev[["contribution"]])
  expect_gt(w_dev[["development"]], w_imm[["development"]])
})

test_that("decision layer does not invent learned club utility claim", {
  df <- data.frame(
    score_contribution_index = 70,
    score_projected_mls = 70,
    score_role_fit = 60,
    score_development = 80,
    score_club_fit = 55,
    score_financial_value = 50,
    score_pathway_fit = 50,
    score_feasibility = 70,
    score_model_uncertainty = 30,
    score_risk = 30
  )
  out <- apply_decision_layer(df, priority = "development", spec = spec)
  expect_match(out$decision_layer_note, "Configurable")
  expect_true(is.finite(out$score_overall[[1]]))
})

test_that("time split labels follow model_spec", {
  panel <- data.frame(season_year = c(2022, 2023, 2024, 2025, 2026), outcome_next = 1)
  panel <- assign_time_splits(panel, spec)
  expect_equal(panel$split[panel$season_year == 2023], "train")
  expect_equal(panel$split[panel$season_year == 2024], "validate")
  expect_equal(panel$split[panel$season_year == 2025], "test")
})
