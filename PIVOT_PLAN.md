# Pivot Plan: 2026 MLS Value Index

## Decision

Pivot `mls-recruitment-intelligence` from a multi-league acquisition engine into a finished portfolio product:

**2026 MLS Value Index — Identifying Undervalued Players in Major League Soccer**

Backup branch: `archive/recruitment-engine-backup` (commit `3da9a34`).
Working branch: `mls-value-index`.

## Primary methodology (locked)

**Position-Adjusted Sporting Impact** = position-group percentile of reliability-adjusted, blended **total Goals Added per 96**.

Rationale:

- Total G+ already aggregates shooting, passing, receiving, dribbling, interrupting, and fouling.
- Additional expert position weights would re-weight the same information and are unvalidated.
- Components are retained for **explanation and visualization only**, not for the primary score.

Formulas:

```
adjusted_gplus_p90 = EB_shrink(observed_gplus_p90 | position prior, m0 = 600)
adjusted_gplus_p96 = adjusted_gplus_p90 × 96/90
Sporting Impact    = percentile(adjusted_gplus_p96 | MLS × period × position_group)
Compensation Pct   = percentile(2026 guaranteed compensation | MLS × position_group)
Value Surplus      = Sporting Impact − Compensation Pct
Undervaluation Score = percentile(Value Surplus | eligible peers in position_group)
```

## Reuse

- ASA/MLSPA collect + clean (`R/collect`, `R/clean`, `scripts/01–02`)
- Evaluation blend scaffolding (`R/features/evaluation_periods.R`) — extended for G+ components
- EB shrinkage helpers (`empirical_bayes_shrink`, `percentile_rank`)
- Provenance / data cutoff (`R/utilities/data_provenance.R`)
- Shiny patterns for profile, compare, download, hot-reload
- Excel workbook mechanics (rewritten for value sheets)

## Archive under `archive/recruitment_engine/`

Club strategy, feasibility, roster needs, incumbent-vs-target, recommendation rules, multi-league translation/cost models, recruitment UI docs, and related tests.

## Delivery order

1. Config + value scoring module
2. Score generation script (MLS-only)
3. New Shiny app (6 pages)
4. SQL schema + data-quality tests
5. Historical validation report
6. Excel export + documentation
7. Deployment workflow + FINAL_RELEASE_CHECKLIST

## Explicit non-goals

Transfer fees, GAM, SBC, trade value, acquisition recommendations, club strategy packs, MLSNP/USL/international production rankings.
