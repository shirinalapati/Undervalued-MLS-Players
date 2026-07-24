# Missing-metric / coverage tests
# Run: Rscript -e 'testthat::test_file("tests/test_missing_metrics.R")'

library(testthat)

root <- if (basename(getwd()) == "tests") dirname(getwd()) else getwd()
setwd(root)

source("R/utilities/load_project.R")
source("R/utilities/model_spec.R")
source("R/collect/demo_generate.R")
source("R/clean/clean_players.R")
source("R/features/build_features.R")
source("R/models/component_scores.R")

cfg <- load_config()
cfg$project$mode <- "demo"
tmp_demo <- file.path(tempdir(), "mls_ri_miss_demo")
cfg$paths$demo <- tmp_demo
cfg$paths$interim <- file.path(tempdir(), "mls_ri_miss_interim")
cfg$paths$processed <- file.path(tempdir(), "mls_ri_miss_processed")
dir.create(cfg$paths$interim, recursive = TRUE, showWarnings = FALSE)
dir.create(cfg$paths$processed, recursive = TRUE, showWarnings = FALSE)

test_that("role fit does not fill missing with 50", {
  generate_demo_cohort(cfg, n_players = 30, seed = 2, output_dir = tmp_demo)
  players <- clean_demo_players(cfg)
  feats <- build_features(players, cfg)
  # Force low coverage by blanking most role metrics
  pct_cols <- grep("_pct$", names(feats), value = TRUE)
  for (col in pct_cols) {
    if (!col %in% c("npxg_p90_pct", "shots_p90_pct")) feats[[col]] <- NA_real_
  }
  res <- score_role_fit_with_coverage(feats, "pressing_striker")
  expect_true(all(res$coverage < 0.5 | is.na(res$score)))
  expect_true(all(is.na(res$score[res$coverage < 0.5])))
})

test_that("fabricated constants are NA in feature matrix", {
  generate_demo_cohort(cfg, n_players = 20, seed = 3, output_dir = tmp_demo)
  players <- clean_demo_players(cfg)
  feats <- build_features(players, cfg)
  expect_true(all(is.na(feats$aerial_win_pct_pct)))
  expect_true(all(is.na(feats$crosses_p90_pct)))
  expect_true(all(is.na(feats$turnovers_forced_p90_pct)))
})

test_that("recommendation labels are cautious", {
  expect_true(all(recommendation_label(75, 30, 50, 60) == "Priority Review"))
  expect_true(all(recommendation_label(60, 40, 75, 55) == "Development Watch"))
  expect_false(any(recommendation_label(75, 30, 50, 60) == "Pursue"))
})

test_that("continuous age development has no 23/23.1 cliff", {
  a23 <- continuous_age_development(23.0, "FW")
  a231 <- continuous_age_development(23.1, "FW")
  expect_lt(abs(a23 - a231), 5)
})
