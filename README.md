# MLS Recruitment Intelligence

**Heuristic recruitment shortlists for MLS clubs (unvalidated prototype)**

> How can MLS clubs surface Priority Review / Monitor / Development Watch candidates from public data — without presenting unvalidated indices as precise projections, financial surplus, or confirmed acquisition feasibility?

**Source of truth for formulas/labels:** [`config/model_spec.yml`](config/model_spec.yml)  
**Hybrid architecture (learn / configure / rules):** [`docs/architecture.md`](docs/architecture.md)  
**Accuracy audit:** [`docs/ACCURACY_AUDIT.md`](docs/ACCURACY_AUDIT.md)  
**Methodology:** [`docs/methodology.md`](docs/methodology.md)

It is **not** a “best players in the world” ranking. Scores are fixed-reference heuristics; filters change rank, not the underlying index.

## What this demonstrates

| Skill | Where it shows up |
| --- | --- |
| R analysis & modeling | `R/features`, `R/models`, `R/rankings` |
| SQL / relational design | `database/schema.sql`, `R/database` |
| Public API / reproducible acquisition | ASA `itscalledsoccer` client in `R/collect` |
| Interactive scouting software | `app/` (Shiny) |
| Staff-facing reports | `reports/*.qmd`, Excel shortlists in `exports/` |
| Honest methodology | `docs/methodology.md`, `docs/ACCURACY_AUDIT.md` |

## Research design (short)

Players receive transparent **0–100 component scores** (see `model_spec.yml`):

1. Estimated Near-Term Contribution Index (role-specific; unvalidated)  
2. Tactical role fit (coverage-gated; missing = NA, never 50)  
3. Acquisition feasibility (gate)  
4. Development outlook (continuous age curves)  
5. Compensation-Adjusted Value Index (known guaranteed compensation only)  
6. Model uncertainty / sporting volatility / acquisition complexity (split)

Default recruitment priority (from `model_spec`):

- Sporting = 55% contribution + 30% role fit + 15% development  
- Priority = 65% sporting + 15% style + 15% comp-adjusted value + 5% pathway  

**Cautious Rec labels until backtests:** Priority Review · Development Watch · Monitor · Low Priority  

**Risk** displayed as model uncertainty; optionally soft-penalizes overall. Weights are configuration, not truth.

## Primary player pool (MVP)

Realistic MLS recruitment markets first:

- MLS  
- MLS NEXT Pro  
- USL Championship  
- (Extensible) Canadian Premier League, NCAA / SuperDraft, selected accessible international leagues  

Elite Big-5 regular starters are **out of scope** for the MVP unless availability filters are explicitly enabled later.

## MVP tactical roles (5)

Designed so additional roles can be added via `config/role_weights.yml` without rewriting the ranking engine:

1. Pressing striker  
2. Transition winger  
3. Ball-winning midfielder  
4. Progressive center back  
5. Overlapping fullback  

## Architecture

```
One universal player database
+ one ranking framework
+ configurable MLS club recruitment profiles
+ seasonal positional needs
= club-specific shortlists from the same player pool
```

Club profiles are labeled **public-data-based estimated club profiles**. Users can override weights in the Shiny app.

## Repository layout

See the tree under `mls-recruitment-intelligence/`. Pipeline stages live in `scripts/01_…` through `07_…`.

## Quick start

### Prerequisites

- R ≥ 4.3  
- Quarto or pandoc (for HTML reports)  
- Optional: PostgreSQL (SQLite used for local MVP)

### One-command pipeline (recommended)

```bash
cd mls-recruitment-intelligence
Rscript scripts/install_dependencies.R
Rscript scripts/run_pipeline.R
Rscript -e "shiny::runApp('app', port = 7788)"
```

Live ASA mode (requires `itscalledsoccer`):

```bash
Rscript -e "install.packages('itscalledsoccer')"
Rscript scripts/run_pipeline.R --live
```

### Manual step-by-step

```bash
Rscript scripts/00_generate_demo_data.R
Rscript scripts/01_collect_data.R
Rscript scripts/02_clean_data.R
Rscript scripts/03_load_database.R
Rscript scripts/04_build_features.R
Rscript scripts/05_train_models.R
Rscript scripts/06_generate_rankings.R
Rscript scripts/07_generate_reports.R
```

Set `project.mode: live` in `config/config.yml` to pull from the American Soccer Analysis API via `itscalledsoccer` (respect rate limits; results are cached under `data/raw/`).

## Case studies

Narratives in `reports/case_studies/`:

1. High-pressing transition club (e.g. Red Bulls–like profile)  
2. Possession-oriented club  
3. Budget-conscious MLS club  

## Daily refresh (deployed live season)

The product does **not** stream match-by-match. For a deploy where users see a **new data cutoff the next day**:

1. **Schedule a nightly pipeline** (force re-fetch ASA/MLSPA, rebuild scores):

```bash
# cron example — 06:15 local time
15 6 * * * cd /path/to/mls-recruitment-intelligence && Rscript scripts/refresh_daily.R >> logs/refresh.log 2>&1
```

Or manually:

```bash
Rscript scripts/run_pipeline.R --force-refresh --skip-reports
```

2. **Keep Shiny running** — the app polls processed files (default every 60s) and **hot-reloads** when the cutoff/files change. No restart required after a successful refresh.

Config knobs in `config/config.yml`:

- `acquisition.asa.cache_hours` — stale cache threshold (default 12h)  
- `deployment.auto_reload` / `deployment.refresh_check_seconds` — in-app hot-reload  

ASA rate limits still apply; schedule once per day (or a few times), not continuously.

## Status

**Live ASA mode** is the default product path (`itscalledsoccer` → clean mapper → scoring). Cached live pulls land under `data/raw/cache/`. Use `--demo` only for offline synthetic demos.


Primary MVP source: **American Soccer Analysis** public API (`itscalledsoccer` R client) for MLS, MLS NEXT Pro, and USL Championship — including xG, xPass, Goals Added (g+), and MLS salaries.

See `docs/data_sources.md` for the full assessment (licensing, identifiers, reliability, scraping alternatives).

## Important limitations

- League translation factors for MVP are **transparent tier-based priors**, not high-confidence causal estimates.  
- Transfer values and contract status are often incomplete; feasibility uses **tiers**, not exact fees.  
- Club profiles are **estimates from public information**, not internal sporting plans.  
- Rankings are decision-support tools, not signing recommendations.

Full caveats: `docs/limitations.md`.

## License

MIT — see `LICENSE`. Attribute upstream data providers as documented in `docs/data_sources.md`.
