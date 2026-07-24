# COMPLETION_STATUS.md — MLS Recruitment Intelligence

**Last updated:** 2026-07-23  
**Product:** Club-configurable recruitment decision-support platform  
**Rule:** A feature is finished only when sourced, documented, tested, exported, disclosed, production-backed, and reproducible.

**Overall:** Production completion in progress — **not** release-ready.

---

## Status legend

| Tag | Meaning |
| --- | --- |
| Complete | Meets completion rule |
| Partially complete | Core exists; gaps remain |
| Incorrect | Logic/data contradicts product definition |
| Unvalidated | Implemented but no historical validation |
| Blocked by data | Waiting on sources / sample |
| Not started | Absent |

---

## Phase status

| Phase | Topic | Status | Notes |
| --- | --- | --- | --- |
| 1 | One source of truth | Partially complete | Required config files added; About summary from config; Methodology still mixed prose |
| 2 | Production data pipeline | Partially complete | Live ASA; g+ fixed; provenance reconcile; roster designations still interim |
| 3 | Database completion | Partially complete | Schema partial; roster/contract/transfer empty |
| 4 | Sporting contribution | Partially complete / Unvalidated | Real g+ rates; heuristic + ridge scaffold; horizon = next complete MLS season |
| 5 | League translation | Partially complete / Blocked by data | Assumed tiers; movers sample thin |
| 6 | Age / development / reliability | Partially complete | Continuous curves; learned m0/age not fitted |
| 7 | Tactical-role fit | Partially complete | Coverage gates done; proxies remain |
| 8 | Club conditioning | Partially complete | Decision layer exists; double-count reduced |
| 9 | Acquisition feasibility | Partially complete / Blocked by data | Heuristic; no contracts |
| 10 | Compensation-adjusted value | Partially complete | Renamed; not acquisition cost |
| 11 | Uncertainty / risk | Partially complete | Split in scored CSVs; UI exposure partial |
| 12 | Roster needs | Partially complete | Minutes+salary backbone ≠ official designations |
| 13 | Incumbent vs target | Partially complete | Invariants + tests |
| 14 | Recommendation system | Partially complete | Cautious labels; thresholds YAML-driven |
| 15 | Backtesting | Unvalidated | Plan + scaffold; no published metrics |
| 16 | Automated tests | Partially complete | 4 test files green |
| 17 | Office exports | Partially complete | Excel partial; Word/PPT not started |
| 18 | UI completion | Partially complete | About cleaned; polish deferred |
| 19 | Documentation | Partially complete | Audits + validation plan added |
| 20 | Reproducibility / deploy | Partially complete | renv.lock without full renv; no deploy |
| 21 | Final release audit | Not started | Checklist created as living document |

---

## Prioritized issue list (active)

### P0 — Incorrect / contradictory (do first)

1. ~~**[P0]** Provenance synthetic vs live CSVs~~ **Fixed** — `reconcile_provenance_with_data()`  
2. ~~**[P0]** `goals_added_p90` entirely zero~~ **Fixed** — ASA `goals_added_raw` + Interrupting mapping  
3. ~~**[P0]** About hardcodes conflicting Overall formula~~ **Fixed** — `render_methodology_summary()`  
4. ~~**[P0]** Stale component-score CSVs~~ **Fixed** — rebuilt with hybrid columns  

### P1 — Data integrity / sources (next)

5. Official roster designations + transactions (not minutes-only membership).  
6. International roster status from official sources only.  
7. Separate model-training cutoff when artifacts exist.  
8. Prevent demo tests from leaving production provenance dirty (isolate paths — partially done).

### P2 — Model validity

9. Define single forecast horizon; fit ridge contribution with time split; intervals + baselines.  
10. Fit / disclose league translation; assumed priors when blocked.  
11. Fit age curves + metric-specific shrinkage.

### P3 — Recommendations / validation / tests / exports / docs / deploy

12. Expand invariant + filter-stability tests.  
13. Real backtests → `reports/model_validation.html`, `docs/validation.md`, model card.  
14. Complete Excel sheets; add Word/PDF + PowerPoint from same engine.  
15. Finish docs list; one-command rebuild; deploy without silent synthetic fallback.

---

## Completed this session (rolling)

- [x] Repository audit  
- [x] `COMPLETION_STATUS.md` (this file)  
- [x] `docs/ACCURACY_AUDIT.md` (refresh pointer)  
- [x] `docs/DATA_SOURCE_AUDIT.md`  
- [x] `docs/VALIDATION_PLAN.md`  
- [x] `docs/validation.md`  
- [x] `FINAL_RELEASE_CHECKLIST.md` (living)  
- [x] Phase 1 config SoT: `model.yml`, `roles.yml`, `thresholds.yml`, `recommendation_rules.yml`, `data_sources.yml`, `export_settings.yml`  
- [x] About page driven from `render_methodology_summary()` (no competing Overall formula dump)  
- [x] **P0** Goals Added mapping fixed (`goals_added_raw` + Interrupting→defending)  
- [x] **P0** Provenance reconcile — live ASA no longer flagged synthetic when data is live  
- [x] Rebuilt interim + features + component scores with hybrid columns; `is_synthetic=FALSE`  
- [ ] Official roster designations + transactions ingestion  
- [ ] Fitted contribution ridge + published backtest metrics  
- [ ] Word/PPT exports  
- [ ] Deployment  

### Phase status updates (this session)

| Phase | Was | Now |
| --- | --- | --- |
| 1 Source of truth | Partially | Partially (required files exist; Methodology tab still prose) |
| 2 Data pipeline | Incorrect | Partially (g+ fixed; provenance reconcile; roster designations still interim) |
| 4 Contribution | Partially | Partially (real g+ flowing; still heuristic/unvalidated) |
| 15 Backtesting | Unvalidated | Unvalidated (plan + unit invariants only) |

---

## How to update this file

After each completed task: mark the phase row, check an item under “Completed this session,” and move issues from P0→done only when the completion rule is met (not merely “renders in Shiny”).
