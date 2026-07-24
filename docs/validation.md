# docs/validation.md

**Status:** Unvalidated — historical metrics not yet published.  
**Plan:** [`VALIDATION_PLAN.md`](VALIDATION_PLAN.md)  
**Scaffold:** `docs/validation/backtest_summary.json` · `scripts/run_backtests.R` · `scripts/08_train_learned_models.R`

Until this page contains test-season MAE/RMSE/rank correlation vs baselines, the live app must show status **unvalidated_heuristic** and use cautious recommendation labels only.

## Forecast horizon (locked)

**Next complete MLS season** (`config/thresholds.yml` / `config/model.yml`).

Sporting contribution excludes salary, cost, budget, and feasibility.

## Results

| Evaluation | Status |
| --- | --- |
| Contribution backtest | Pending |
| League-mover translation | Pending / assumed priors |
| Shortlist hit-rate | Pending |
| Stability | Pending |
| Recommendation invariants (unit) | Passing (`tests/test_recommendation_invariants.R`) |

## How to regenerate

```bash
Rscript scripts/08_train_learned_models.R
Rscript scripts/run_backtests.R
```
