# VALIDATION_PLAN.md

**Status:** Required for project completion — currently **Unvalidated** (scaffold only).  
**Related:** `scripts/run_backtests.R`, `scripts/08_train_learned_models.R`, `docs/validation/backtest_summary.json`

---

## Principles

1. **Time-aware only** — no future-data leakage.  
2. **Baselines required** — learned/heuristic models must beat simple alternatives or disclose failure.  
3. **Hybrid taxonomy** — validate learned outcomes, configurable decision sensitivity, and recommendation **rules** separately.  
4. **Cautious language** until metrics are published in-app and in `docs/validation.md`.

---

## Forecast horizon (to lock in Phase 4)

**Proposed primary horizon (pending confirmation in `config/model.yml`):**

> **Next complete MLS season contribution** (e.g. predict 2025 from ≤2024 cutoff; live 2026 product trains only on data available before cutoff).

Secondary reporting: next-12-month and remaining-season — not mixed into one unlabeled index.

**Target (document exact choice when fitted):**

- Primary: future Goals Added per 90 (or total g+ with minutes control)  
- Secondary: above-average MLS contributor (binary)

**Must exclude from sporting model:** salary, cost tier, budget, financial value, feasibility.

---

## A. Sporting contribution validation

| Metric | By |
| --- | --- |
| MAE, RMSE | position, source league, minutes band |
| Spearman rank correlation | same |
| Calibration | deciles |
| R² | where useful |

**Baselines:** previous-season contribution; raw g+/90; raw (xG+xA)/90; age-only; minutes-only; league-average.

**Split:** train ≤2023 · validate 2024 · test 2025 · live cutoff-safe.

---

## B. League translation validation

Movers: USLC→MLS, MLSNP→MLS (and other leagues when available).

Report: pre/post change, age/position/minutes controls, sample n, CIs.  
If n insufficient: assumed priors + low confidence (no claimed precision).

---

## C. Shortlist validation

At historical cutoffs: top-k shortlists → later MLS minutes / contribution / hit rate vs simple rankings (g+, xG+xA, age, salary, minutes).

---

## D. Stability

Bootstrap minutes / perturb inputs: top-10 retention, rank change, recommendation flip rate.  
Decision-layer: changing club weights changes **rank**, not Model-1 contribution scores.

---

## E. Recommendation invariants

Automated tests (expand):

- More expensive ≠ lower-cost  
- Weaker contribution ≠ immediate upgrade without margins  
- Unknown cost ≠ cost certainty  
- Missing metrics ≠ high similarity / zero delta  
- Narrative agrees with deltas  

---

## Deliverables

| Artifact | Status |
| --- | --- |
| `docs/validation/backtest_summary.json` | Scaffold |
| `reports/model_validation.html` | Not started |
| `docs/model_card.md` / `reports/model_card.md` | Not started |
| `docs/validation.md` | This plan → results section TBD |
| In-app validation summary | Not started |

---

## Pass criteria (release)

- Contribution model metrics published for test season with baselines  
- Shortlist hit-rate vs ≥2 baselines  
- All recommendation invariant tests green  
- Limitations section lists failures honestly  
- App shows model version + validation status badge (Unvalidated / Limited / Validated)
