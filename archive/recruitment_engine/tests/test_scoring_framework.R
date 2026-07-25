# test_scoring_framework.R
# Run: Rscript -e 'testthat::test_file("tests/test_scoring_framework.R")'

library(testthat)

root <- if (basename(getwd()) == "tests") dirname(getwd()) else getwd()
setwd(root)

source("R/utilities/load_project.R")
source("R/collect/demo_generate.R")
source("R/clean/clean_players.R")
source("R/features/build_features.R")
source("R/models/component_scores.R")

cfg <- load_config()
cfg$project$mode <- "demo"

# Isolate demo writes so tests never overwrite portfolio demo data
tmp_demo <- file.path(tempdir(), "mls_ri_test_demo")
cfg$paths$demo <- tmp_demo
cfg$paths$interim <- file.path(tempdir(), "mls_ri_test_interim")
dir.create(cfg$paths$interim, recursive = TRUE, showWarnings = FALSE)

test_that("demo cohort generates expected columns", {
  demo <- generate_demo_cohort(cfg, n_players = 40, seed = 1, output_dir = tmp_demo)
  expect_true(nrow(demo$players) == 40)
  expect_true(all(c("npxg_p90", "salary", "league_id") %in% names(demo$players)))
})

test_that("role fit is within 0-100 and role-specific", {
  players <- clean_demo_players(cfg)
  feats <- build_features(players, cfg)
  s1 <- score_role_fit(feats, "pressing_striker")
  s2 <- score_role_fit(feats, "ball_winning_midfielder")
  expect_true(all(s1 >= 0 & s1 <= 100))
  expect_true(all(s2 >= 0 & s2 <= 100))
  expect_false(isTRUE(all.equal(s1, s2)))
})

test_that("overall weights are configurable", {
  players <- clean_demo_players(cfg)
  feats <- build_features(players, cfg)
  scored <- compute_component_scores(feats, "transition_winger", cfg)
  a <- apply_overall_score(scored, list(
    projected_mls_performance = 1, tactical_role_fit = 0, financial_value = 0,
    acquisition_feasibility = 0, development_upside = 0
  ), apply_risk = FALSE)
  b <- apply_overall_score(scored, list(
    projected_mls_performance = 0, tactical_role_fit = 0, financial_value = 0,
    acquisition_feasibility = 1, development_upside = 0
  ), apply_risk = FALSE)
  expect_false(isTRUE(all.equal(a$score_overall, b$score_overall)))
})

test_that("elite stars are not required — feasibility penalizes high cost tiers", {
  players <- clean_demo_players(cfg)
  feats <- build_features(players, cfg)
  scored <- compute_component_scores(feats, "pressing_striker", cfg)
  hi <- mean(scored$score_feasibility[scored$cost_tier >= 4], na.rm = TRUE)
  lo <- mean(scored$score_feasibility[scored$cost_tier <= 2], na.rm = TRUE)
  expect_true(lo > hi)
})
