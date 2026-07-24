# Metric Dictionary

All per-90 metrics are computed only after the minimum-minutes threshold unless noted.

## Identity & context

| Field | Definition |
| --- | --- |
| `player_id` | Internal UUID / ASA id |
| `season` | Competition season year |
| `league_id` | `mls`, `mlsnp`, `uslc`, … |
| `position_group` | FW, W, CM, FB, CB, GK |
| `minutes` | Minutes played in competition season |
| `age` | Age as of July 1 of season year (approx if DOB missing) |

## Attacking / finishing

| Metric | Definition | Primary source |
| --- | --- | --- |
| `npxg_p90` | Non-penalty expected goals per 90 | ASA xG |
| `shots_p90` | Shots per 90 | ASA / derived |
| `goals_p90` | Goals per 90 | ASA |

## Creation

| Metric | Definition | Primary source |
| --- | --- | --- |
| `xa_p90` | Expected assists / key creation proxy per 90 | ASA xPass / xG assisted fields when available |
| `progressive_passes_p90` | Progressive passes per 90 (proxy from g+/passing) | ASA g+ / FBref later |
| `xpass_completion_above_expected` | Completion vs difficulty | ASA xPass |

## Ball progression

| Metric | Definition |
| --- | --- |
| `progressive_carries_p90` | Progressive carries per 90 (proxy) |
| `carries_into_final_third_p90` | Final-third carries (proxy) |
| `progressive_runs_p90` | Attacking runs / carry proxy for forwards |

## Defensive / pressing proxies

| Metric | Definition |
| --- | --- |
| `pressures_p90` | Defensive pressure actions proxy (g+ defending / derived) |
| `tackles_p90` | Tackles per 90 |
| `interceptions_p90` | Interceptions per 90 |
| `defensive_actions_p90` | Combined defensive actions |
| `turnovers_forced_p90` | Forced turnovers proxy |
| `aerial_win_pct` | Aerial wins / aerial duels |

## Retention / possession

| Metric | Definition |
| --- | --- |
| `ball_retention` | Proxy from pass completion and dispossessions avoided |
| `pass_completion_pct` | Passes completed / attempted |
| `pass_completion_under_pressure` | Proxy from xPass difficulty splits when available |
| `successful_dribbles_p90` | Take-on success volume proxy |

## Transition / wide play

| Metric | Definition |
| --- | --- |
| `transition_involvement` | Composite of progressive + defensive regain proxies |
| `crosses_p90` | Crosses per 90 |
| `touches_att_third_p90` | Attacking-third touches |

## Goals Added (g+)

ASA Goals Added action types are unnested and mapped into the proxies above. Exact mapping lives in `R/features/map_asa_metrics.R`.

## Model outputs

| Score | Range | Meaning |
| --- | --- | --- |
| `score_projected_mls` | 0–100 | Expected MLS contribution |
| `score_role_fit` | 0–100 | Match to selected tactical role |
| `score_feasibility` | 0–100 | Acquisition realism |
| `score_development` | 0–100 | Upside / trajectory |
| `score_financial_value` | 0–100 | Contribution vs cost |
| `score_risk` | 0–100 | Higher = more risk |
| `score_overall` | 0–100 | Configurable weighted blend |
| `confidence` | low/medium/high | Sample + data quality |
| `data_quality` | 0–100 | Metric coverage & identity confidence |

## Recommendation labels

| Label | Meaning |
| --- | --- |
| Pursue | High overall, acceptable risk, fits need |
| Monitor | Interesting but wait on minutes/contract/price |
| Development target | Upside-driven; may not help immediately |
| Pass | Poor fit, unattainable, or adverse risk/value |
