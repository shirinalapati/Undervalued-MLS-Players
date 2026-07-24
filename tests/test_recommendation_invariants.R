# Recommendation invariant tests
# Run: Rscript -e 'testthat::test_file("tests/test_recommendation_invariants.R")'

library(testthat)

root <- if (basename(getwd()) == "tests") dirname(getwd()) else getwd()
setwd(root)

source("R/utilities/load_project.R")
source("R/utilities/model_spec.R")
source("R/features/build_features.R")
source("R/models/component_scores.R")
source("R/models/roster_comparison.R")

make_player <- function(name, contrib, role_fit, age, salary, cost_tier,
                        development = 50, feasibility = 70, confidence = "medium",
                        compensation_known = TRUE, ...) {
  extras <- list(...)
  row <- data.frame(
    display_name = name,
    asa_player_id = paste0("id_", gsub("\\s", "_", name)),
    score_projected_mls = contrib,
    score_contribution_index = contrib,
    score_role_fit = role_fit,
    score_development = development,
    score_feasibility = feasibility,
    score_financial_value = if (isTRUE(compensation_known) && is.finite(cost_tier)) {
      clip(contrib - c(20, 40, 60, 80, 95)[cost_tier] + 50, 0, 100)
    } else {
      NA_real_
    },
    score_risk = 40,
    score_model_uncertainty = 40,
    confidence = confidence,
    age = age,
    salary = salary,
    cost_tier = cost_tier,
    compensation_known = compensation_known,
    league_id = "mls",
    minutes = 1500,
    minutes_share = 0.5,
    score_pathway_fit = 60,
    npxg_p90_pct = 60,
    xa_p90_pct = 55,
    pressures_p90_pct = 50,
    shots_p90_pct = 58,
    defensive_actions_p90_pct = 45,
    tackles_p90_pct = 40,
    interceptions_p90_pct = 42,
    progressive_passes_p90_pct = 50,
    progressive_carries_p90_pct = 48,
    ball_retention_pct = 50,
    progressive_passes_received_p90_pct = 50,
    carries_into_final_third_p90_pct = 48,
    successful_dribbles_p90_pct = 48,
    pass_completion_under_pressure_pct = 50,
    pass_completion_pct_pct = 50,
    turnovers_forced_p90_pct = NA_real_,
    progressive_runs_p90_pct = NA_real_,
    transition_involvement_pct = NA_real_,
    fouls_won_ratio_pct = NA_real_,
    aerial_win_pct_pct = NA_real_,
    crosses_p90_pct = NA_real_,
    long_pass_completion_pct_pct = NA_real_,
    touches_att_third_p90_pct = NA_real_,
    video_questions = "n/a",
    stringsAsFactors = FALSE
  )
  for (nm in names(extras)) row[[nm]] <- extras[[nm]]
  row
}

test_that("more expensive target cannot be labeled lower-cost", {
  inc <- make_player("Incumbent", contrib = 60, role_fit = 60, age = 28,
                     salary = 400000, cost_tier = 3L)
  tgt <- make_player("Expensive Target", contrib = 62, role_fit = 60, age = 27,
                     salary = 900000, cost_tier = 4L)
  cmp <- compare_incumbent_target(inc, tgt, "pressing_striker", objective = "lower_cost")
  expect_false(grepl("Lower-cost", cmp$relationship, ignore.case = TRUE))
  expect_equal(cmp$relationship, "No clear recruitment advantage identified")
})

test_that("weaker contribution cannot be labeled immediate upgrade", {
  inc <- make_player("Incumbent", contrib = 70, role_fit = 65, age = 26,
                     salary = 500000, cost_tier = 3L)
  tgt <- make_player("Weaker", contrib = 55, role_fit = 70, age = 25,
                     salary = 450000, cost_tier = 3L, confidence = "high")
  cmp <- compare_incumbent_target(inc, tgt, "pressing_striker", objective = "upgrade")
  expect_false(grepl("Immediate upgrade|Upgrade / replacement", cmp$relationship))
  expect_equal(cmp$relationship, "No clear recruitment advantage identified")
})

test_that("missing financial data cannot trigger financial recommendation", {
  inc <- make_player("Incumbent", contrib = 60, role_fit = 60, age = 28,
                     salary = 400000, cost_tier = 3L)
  tgt <- make_player("Unknown Pay", contrib = 65, role_fit = 60, age = 24,
                     salary = NA_real_, cost_tier = NA_integer_,
                     compensation_known = FALSE)
  cmp <- compare_incumbent_target(inc, tgt, "pressing_striker", objective = "lower_cost")
  expect_false(grepl("Lower-cost", cmp$relationship, ignore.case = TRUE))
  expect_true(is.na(cmp$scores$financial_efficiency))
  expect_match(cmp$narrative, "Compensation unknown|no financial recommendation", ignore.case = TRUE)
})

test_that("valid lower-cost alternative passes invariants", {
  inc <- make_player("Incumbent", contrib = 60, role_fit = 60, age = 28,
                     salary = 800000, cost_tier = 4L)
  tgt <- make_player("Cheaper Peer", contrib = 58, role_fit = 62, age = 26,
                     salary = 250000, cost_tier = 2L)
  cmp <- compare_incumbent_target(inc, tgt, "pressing_striker", objective = "lower_cost")
  expect_match(cmp$relationship, "Lower-cost")
  expect_true(cmp$deltas$cost_tier_cheaper > 0)
  expect_true(as.numeric(tgt$salary) < as.numeric(inc$salary))
})

test_that("written recommendation agrees with displayed deltas", {
  inc <- make_player("Incumbent", contrib = 50, role_fit = 55, age = 29,
                     salary = 600000, cost_tier = 3L)
  tgt <- make_player("Upgrade", contrib = 70, role_fit = 70, age = 26,
                     salary = 650000, cost_tier = 3L, confidence = "high")
  cmp <- compare_incumbent_target(inc, tgt, "pressing_striker", objective = "upgrade")
  expect_match(cmp$relationship, "Immediate upgrade")
  expect_gte(cmp$deltas$projected_contribution, 8)
  expect_match(cmp$narrative, "Immediate upgrade")
  # Narrative contribution delta should match
  expect_true(cmp$deltas$projected_contribution == cmp$summary_row$delta_projected)
})

test_that("both-missing metrics do not produce zero delta", {
  inc <- make_player("Incumbent", contrib = 60, role_fit = 60, age = 28,
                     salary = 400000, cost_tier = 3L)
  tgt <- make_player("Target", contrib = 62, role_fit = 61, age = 27,
                     salary = 380000, cost_tier = 3L)
  cmp <- compare_incumbent_target(inc, tgt, "pressing_striker", objective = "upgrade")
  both <- cmp$metric_delta[cmp$metric_delta$both_missing %in% TRUE, , drop = FALSE]
  if (nrow(both)) {
    expect_true(all(is.na(both$delta)))
  }
  expect_true(TRUE)
})
