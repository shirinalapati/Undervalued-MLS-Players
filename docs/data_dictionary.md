# Data dictionary (Value Index)

| Field | Definition |
|-------|------------|
| `sporting_impact` | Position-Adjusted Sporting Impact (0–100) |
| `compensation_percentile` | Position pay percentile (0–100) |
| `compensation_percentile_league` | League-wide pay percentile |
| `value_surplus` | Impact − compensation percentile (−100–100) |
| `undervaluation_score` | Positional percentile of surplus among eligible |
| `display_rank` | Sequential rank across all official eligible players (1 = best) |
| `position_rank` | Rank within position among eligible (1 = best) |
| `undervaluation_rank` | Alias of `position_rank` (legacy column name) |
| `adjusted_goals_added_p96` | EB-shrunk blended total G+ scaled to per 96 |
| `goals_added_*_p96` | Explanatory G+ components per 96 minutes (visible unit) |
| `goals_added_*_p90` | Source ASA component rates (converted ×96/90 for display) |
| `pct_goals_added_*` | Component positional percentiles |
| `data_confidence` | High / Medium / Low / Insufficient |
| `model_uncertainty` | 0–100 statistical uncertainty |
| `model_confidence` | 100 − model_uncertainty |
| `value_label` | Elite Value, Strong Value, Undervalued, Fair Value, Below Expected Value by Current Model, Small-Sample Watchlist, Insufficient Evidence |
| `official_eligible` | Meets official ranking gates |
| `minutes_2026` | 2026 minutes |
| `compensation` | 2026 guaranteed compensation (USD) |
