# 2026 MLS Value Index

## Identifying Undervalued Players in Major League Soccer

**Research question:** Which current MLS players provide the most compensation-efficient position-adjusted on-ball impact?

This portfolio project ranks **current MLS players only** using:

- 2026 season-to-date ASA performance
- 2025 full-season performance as a stabilizing prior
- 2026 MLSPA guaranteed compensation

The production model measures **position-adjusted on-ball impact through Goals Added only**. It does not estimate adjusted team impact, off-ball contribution, plus-minus, transfer fees, GAM, Salary Budget Charge, or trade value.

## Core formulas

```
Sporting Impact         = position percentile of reliability-adjusted blended total G+/96
Compensation Percentile = position percentile of 2026 guaranteed compensation
Value Surplus           = Sporting Impact − Compensation Percentile
Undervaluation Score    = position percentile of Value Surplus among eligible players
```

Goals Added components (shooting, passing, receiving, dribbling, interrupting, fouling) explain profiles — they are **not** re-weighted into the primary Sporting Impact score. All visible Goals Added rates use **per 96 minutes**.

## Official eligibility

- Current MLS player with a valid position group
- Known 2026 guaranteed compensation
- Sporting Impact available
- ≥ 450 minutes in 2026

Goalkeepers are excluded until comparable public metrics support a separate model.

Players evaluated and official eligible counts are generated dynamically from production score files (see About page, banner, Excel Executive Summary, and `data/processed/value_index_summary.json`).

## Value labels

No player with Value Surplus ≤ 0 can be labeled Undervalued, Strong Value, or Elite Value.

| Label | Requirements |
|-------|----------------|
| Elite Value | Surplus ≥ 25, Undervaluation Score ≥ 90, Sporting Impact ≥ 70, Medium/High confidence |
| Strong Value | Surplus ≥ 15, Undervaluation Score ≥ 75, Sporting Impact ≥ 60, Medium/High confidence |
| Undervalued | Surplus ≥ 5, Undervaluation Score ≥ 60, Sporting Impact ≥ 55 |
| Fair Value | Surplus in (−15, 5), or fails an impact/confidence floor for a higher label |
| Below Expected Value by Current Model | Surplus ≤ −15 |

## Ranking columns

- **Display Rank** — sequential across all official eligible players (combined table)
- **Position Rank** — rank within position group (Position Rankings and combined table)

## Application pages

1. About & Methodology  
2. MLS Value Rankings  
3. Position Rankings  
4. Team Value  
5. Player Profile  
6. Compare Players  

## Quick start

```bash
cd mls-recruitment-intelligence   # repository folder
Rscript scripts/install_dependencies.R
Rscript scripts/run_pipeline.R --skip-collect --skip-reports   # score from existing interim data
# or full refresh:
# Rscript scripts/run_pipeline.R --force-refresh

Rscript -e "shiny::runApp('app', port = 7788)"
```

One-command style workflow:

```bash
Rscript scripts/run_pipeline.R --skip-collect
Rscript -e "shiny::runApp('app', host='0.0.0.0', port=7788)"
```

## Pipeline steps

1. Restore dependencies  
2. Collect ASA / MLSPA (MLS-only)  
3. Clean + load SQLite  
4. Build Value Index scores  
5. Run tests + validation  
6. Export Excel  
7. Launch Shiny  

## Data cutoffs

Displayed in-app from `data/processed/data_provenance.json` (performance through date and MLSPA compensation as-of date).

## Validation

See `docs/validation.md` and `reports/_output/model_validation.html`.  
Primary claim remains descriptive: players whose public on-ball impact exceeds compensation standing. Year-to-year stability is reported; breakout prediction is not claimed.

## Documentation

- `docs/methodology.md`
- `docs/model_card.md`
- `docs/data_sources.md`
- `docs/limitations.md`
- `docs/deployment.md`
- `FINAL_RELEASE_CHECKLIST.md`
- `PIVOT_PLAN.md`

Recruitment-engine code is preserved under `archive/recruitment_engine/` and branch `archive/recruitment-engine-backup`.
