# Technical methodology

This document is the **technical reviewer** companion to the Shiny About page.

The application UI explains the product in plain English.  
This file documents **exact production formulas**, configuration values, implementation locations, and variable definitions.

**Primary config source of truth:** [`config/model.yml`](../config/model.yml)  
**Also loaded:** [`config/config.yml`](../config/config.yml), [`config/thresholds.yml`](../config/thresholds.yml), [`config/league_tiers.yml`](../config/league_tiers.yml), [`config/roles.yml`](../config/roles.yml) / [`config/role_weights.yml`](../config/role_weights.yml), [`config/recommendation_rules.yml`](../config/recommendation_rules.yml)

Related docs: [`architecture.md`](architecture.md), [`ACCURACY_AUDIT.md`](ACCURACY_AUDIT.md), [`validation.md`](validation.md)

---

## Status

| Field | Current value |
| --- | --- |
| Model version | `0.3.0` (from `model.yml`) |
| Product status | `unvalidated_heuristic` |
| Contribution path | `hybrid.learned.contribution.status = fallback_heuristic` |
| Shrinkage | `fallback_fixed_m0` with `default_m0 = 600` |
| League translation | `assumed_tier_priors` |
| Age curves | `fallback_priors` |

Do **not** present indices as precise projections, financial surplus, or confirmed acquisition feasibility until backtests pass.

---

## Pipeline

```
Collect → Clean / identity standardize → SQL load → Feature engineering
→ Empirical Bayes shrinkage → Assumed league-strength adjustments
→ MLS reference percentiles → Component scores → Club-conditioned ranking
→ Shiny / Excel outputs
```

Implementation entry points:

- Features: `R/features/build_features.R` → `build_features()`
- Shrinkage: `R/utilities/load_project.R` → `empirical_bayes_shrink()`; m0 via `R/models/learned/shrinkage_model.R` → `shrinkage_m0_for()`
- Scores: `R/models/component_scores.R`
- Decision layer: `R/models/decision_layer.R` → `club_utility_score()` / `apply_decision_layer()`
- Roster needs: `R/models/roster_analysis.R`
- Comparisons: `R/models/roster_comparison.R`

---

## Sample-size shrinkage (production)

### Formula

```
reliability_weight_i = minutes_i / (minutes_i + prior_strength)
prior_weight_i       = prior_strength / (minutes_i + prior_strength)
adjusted_rate_i      = reliability_weight_i * observed_rate_i
                     + prior_weight_i * prior_rate
```

Identity: `reliability_weight_i + prior_weight_i = 1`.

Equivalent production line in `empirical_bayes_shrink()`:

```r
w <- minutes / (minutes + m0)
w * rate + (1 - w) * prior_rate
```

`prior_strength` (`m0`) is the number of minutes controlling how quickly the model trusts the player’s own rate.

### Metrics shrunk

`npxg_p90`, `xa_p90`, `pressures_p90`, `tackles_p90`, `interceptions_p90`, `progressive_passes_p90`, `progressive_carries_p90`, `goals_added_p90`, `shots_p90`, `crosses_p90`

### Current prior_strength by metric

All listed metrics currently use **`prior_strength = 600`** minutes from `config/model.yml` → `hybrid.learned.shrinkage.default_m0` (via `shrinkage_m0_for()`).  
If a learned artifact exists, per-metric m0 from that artifact wins.

### Definitions

| Symbol | Unit | Range | Higher/lower | Source | Type | Reference population | Missing | Current config |
| --- | --- | --- | --- | --- | --- | --- | --- | --- |
| `minutes_i` | minutes | ≥ 0 | More → higher reliability weight | ASA / processed | Observed | Player-season | NA propagates | — |
| `prior_strength` (m₀) | minutes | > 0 | Higher → more prior | `model.yml` / learned artifact | Configured (learnable) | — | Fallback 600 | **600** |
| `observed_rate_i` | metric units | metric-specific | Usually higher better | ASA | Observed | Player | NA kept | — |
| `prior_rate` | metric units | metric-specific | Same as metric | Group mean | Estimated | `league_id × position_group` | NA → NA adjusted | Computed each build |
| `reliability_weight_i` | fraction | [0,1] | Higher with minutes | Derived | Derived | — | — | Sums to 1 with prior_weight |
| `prior_weight_i` | fraction | [0,1] | Higher with m₀ | Derived | Derived | — | — | `1 − reliability_weight` |
| `adjusted_rate_i` | metric units | metric-specific | Same as metric | Derived | Estimated | Same as prior | NA if inputs NA | — |

### Numeric example

`minutes_i = 900`, `prior_strength = 600`, `observed_rate_i = 0.40`, `prior_rate = 0.30`  
→ `reliability_weight_i = 0.600`, `prior_weight_i = 0.400`, `adjusted_rate_i = 0.360`.

### Evaluation-period blend (related)

```
blend_weight_2026 = clamp(min_ytd_share, max_ytd_share,
                          minutes_2026 / (minutes_2026 + blend_prior_strength))
blend_weight_2025 = 1 - blend_weight_2026
```

Current (`config.yml`): `blend_prior_strength = 700`, `min_ytd_share = 0.15`, `max_ytd_share = 0.90`.

---

## League adjustment (production)

Applied in `build_features()` **after** shrinkage.

```
proj_npxg_i      = shrunk_npxg_i * translation_factor_attack(league_i)
proj_xa_i        = shrunk_xa_i * translation_factor_creation(league_i)
proj_pressures_i = shrunk_pressures_i * translation_factor_defense(league_i)
proj_gplus_i     = shrunk_gplus_i * mean(attack, creation, defense factors)
```

Unknown league → factor `0.70`, uncertainty `0.25`.

### Current league factors (`config/league_tiers.yml`)

| League | Attack | Creation | Defense | Uncertainty |
| --- | ---: | ---: | ---: | ---: |
| mls | 1.00 | 1.00 | 1.00 | 0.05 |
| mlsnp | 0.72 | 0.75 | 0.80 | 0.18 |
| uslc | 0.78 | 0.80 | 0.82 | 0.15 |
| usl1 | 0.65 | 0.68 | 0.72 | 0.22 |
| cpl | 0.70 | 0.72 | 0.75 | 0.20 |
| ncaa | 0.55 | 0.58 | 0.60 | 0.30 |
| international_mid | 0.85 | 0.85 | 0.88 | 0.20 |
| elite_big5 | 1.15 | 1.12 | 1.10 | 0.25 |

Label: **Assumed league-strength adjustments** (not learned mover estimates until fitted).

---

## Role Fit and metric coverage (production)

`score_role_fit_with_coverage()` in `R/features/build_features.R`.

```
coverage_i = sum(|w_j| for observed j) / sum(|w_j| for all role metrics j)
if coverage_i < role_fit_unavailable_below or no observed metrics:
  RoleFit_i = NA
else:
  RoleFit_i = sum( (w_j / sum(w_observed)) * percentile_j ) for observed j
```

### Coverage gates (`thresholds.yml` / `model.yml`)

| Rule | Value |
| --- | ---: |
| Role Fit unavailable below | **0.50** |
| Max confidence Low below | **0.70** |
| Max confidence Medium below | **0.85** |

Missing metrics stay **NA** (never filled with 50). Role weights: `config/roles.yml`.

---

## Estimated Near-Term Contribution (production)

Exposed as `score_projected_mls` / contribution index.

While `contribution.status = fallback_heuristic`:

```
raw_i = weighted_mean(observed contribution-blend percentiles for role)
      - 0.15 * (penalty_blend - 50)   # negative-weight terms only, if any
minutes_factor_i = clip(minutes_i / 2500, 0.7, 1.0)
Contribution_i = clip(raw_i * minutes_factor_i, 0, 100)
```

Fallback if no role columns resolve:

```
raw_i = 0.40*pct_proj_npxg + 0.35*pct_proj_xa + 0.25*pct_proj_press
```

Role blends: `config/model.yml` → `contribution_by_role`.  
Age is **not** applied inside Contribution (Development only).

---

## Compensation-Adjusted Value (production)

`score_compensation_adjusted_value()` in `R/models/component_scores.R`.

```
cost_penalty_i = {1→20, 2→40, 3→60, 4→80, 5→95}
if cost_tier unknown or Contribution non-finite:
  Value_i = NA
else:
  Value_i = clip(Contribution_i - cost_penalty_i + 50, 0, 100)
```

Not transfer fee, salary-budget charge, or acquisition surplus.

---

## Model Uncertainty and Overall penalty (production)

`score_model_uncertainty()` (stored as `score_risk` in tables):

```
sample_u_i = 80 if minutes < 700; 55 if < 1200; 35 if < 2000; else 20
translation_u_i = 100 * tf_uncertainty_i
missing_u_i = clip(100 * (1 - role_metric_coverage_i), 0, 100)
ModelUncertainty_i = clip(0.40*sample_u_i + 0.35*translation_u_i + 0.25*missing_u_i, 0, 100)
```

**Lower is better.**

Uncertainty penalty (`config.yml` `scoring.risk_penalty_weight = 0.15`):

```
AdjustedOverall_i = Overall_i * (1 - λ * ModelUncertainty_i / 100)
```

with `λ = 0.15`.

---

## Overall score (production UI path)

`apply_decision_layer()` → `club_utility_score()` in `R/models/decision_layer.R`.

```
raw_i = Σ w_k * component_k
Overall_i = raw_i * (1 - λ * ModelUncertainty_i / 100)
if Feasibility_i < feasibility_gate: Overall_i = min(Overall_i, 54)
Overall_i = clip(Overall_i, 0, 100)
```

Missing components inside `club_utility_score` coalesce to **50** before blending.

### Balanced atomic weights before club nudges

From `sporting_score × recruitment_priority` in `model.yml`:

| Component | Weight before renormalization |
| --- | ---: |
| contribution | 0.65 × 0.55 = **0.3575** |
| role_fit | 0.65 × 0.30 = **0.195** |
| development | 0.65 × 0.15 = **0.0975** |
| style | **0.15** |
| value | **0.15** |
| pathway | **0.05** |
| feasibility | **0** (gate) |

Feasibility gate: **35** (`thresholds.yml` / `model.yml`).

### Documented flat weights (`config.yml` scoring.weights)

Used when an explicit weight vector is passed to `apply_overall_score`:

| Component | Weight |
| --- | ---: |
| Contribution | 0.30 |
| Role Fit | 0.25 |
| Compensation-Adjusted Value | 0.20 |
| Feasibility | 0.15 |
| Development | 0.10 |

---

## Development Upside (production)

`score_development()`:

```
peak = age_peak_for_position(position_group)   # model.yml age_curves
age_curve = 100 * exp(-0.5 * ((age - (peak - 3)) / 4.5)^2)
age_ups = clip(age_curve - min(45, max(0, age - peak) * 6), 5, 95)
Development_i = weighted_mean of available bits:
  age_ups (0.55) + scale_0_100(yoy_delta) (0.25) + scale_0_100(minutes) (0.20)
```

### Current age peaks (`model.yml` age_curves)

| Position | Peak age |
| --- | ---: |
| F | 25.5 |
| W_AM | 25.8 |
| CM | 26.5 |
| FB | 26.8 |
| CB | 27.2 |
| GK | 29.0 |
| default | 26.5 |

---

## Acquisition Feasibility (production)

`score_feasibility()` heuristic:

```
Feasibility_i = 0.35*cost_score + 0.30*league_score + 0.20*minutes_score + 0.15*domestic_score
```

| Sub-score | Mapping |
| --- | --- |
| cost | NA→45; 1→95; 2→80; 3→55; 4→30; else→10 |
| league | uslc→85; mlsnp→80; mls→60; else→50 |
| minutes | MLS & share>0.75 → 45; else 75 |
| domestic | domestic/homegrown→80; international→55; else→60 |

Nationality ≠ MLS international-roster status.

---

## Roster Need Score (production)

`analyze_club_role_needs()` in `R/models/roster_analysis.R`.

```
gap_component = 100 - percentile_among_clubs(raw_component)
NeedScore = clip( Σ w_k * gap_k , 0, 100)
```

### Current weights (`config.yml` roster_needs.weights)

| Component | Weight |
| --- | ---: |
| starter_quality_gap | 0.30 |
| effective_depth_gap | 0.25 |
| succession_risk | 0.15 |
| availability_risk | 0.10 |
| tactical_coverage_gap | 0.10 |
| financial_efficiency_opportunity | 0.10 |

Higher Need Score = greater estimated need. Reference: clubs × same tactical role.

---

## Player similarity and comparison (production)

`compare_incumbent_target()` in `R/models/roster_comparison.R`.

```
RoleSimilarity = 50 + 50 * cosine_similarity(role_pct vectors centered at 50)
  (requires sim_coverage ≥ 0.50; jointly missing metrics excluded)
UpgradePotential = clip(50 + 0.7*ΔContribution + 0.3*ΔRoleFit, 0, 100)
```

### Invariant thresholds (`recommendation_rules.yml`)

| Label | Key thresholds |
| --- | --- |
| Immediate upgrade | ΔContribution ≥ **8**, Role Fit ≥ **55**, confidence ≥ medium |
| Direct replacement | Role similarity ≥ **70** |
| Lower-cost | Both costs known, cheaper, contribution within tolerance |
| Developmental successor | Age gap ≥ **1**, Upside ≥ **60**, Role ≥ **45**, pathway ≥ **40** |
| Complementary | Complementarity ≥ **55** |
| Rotation/depth | Similarity ≥ **55** and ΔContribution < 8 |
| Fallback | **No clear recruitment advantage identified** |

---

## Recommendation score thresholds (`thresholds.yml`)

| Label | Rules |
| --- | --- |
| Priority Review | Overall ≥ 70, Model Uncertainty ≤ 45, Feasibility ≥ 55 |
| Development Watch | Development ≥ 70, Overall ≥ 55, Feasibility ≥ 50 |
| Monitor | Overall ≥ 55, Model Uncertainty ≤ 65 |
| Low Priority | Feasibility < 30 (auto) or no stronger category |

---

## Formula and variable glossary

| Variable | Meaning | Unit | Range | Direction | Source | Type | Reference | Missing | Current config |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
| `minutes_i` | Player minutes | minutes | ≥0 | More → more reliable | ASA | Observed | Player-season | NA propagates | — |
| `prior_strength` / m₀ | Shrinkage strength | minutes | >0 | Higher → more prior | `model.yml` | Configured | — | Fallback 600 | **600** |
| Blend prior_strength | YTD/2025 blend m₀ | minutes | >0 | Higher → slower YTD shift | `config.yml` | Configured | — | — | **700** |
| `observed_rate_i` | Raw per-90 | metric | metric | Usually ↑ better | ASA | Observed | Player | NA kept | — |
| `prior_rate` | Group mean rate | metric | metric | Same | Computed | Estimated | league×position | NA→NA | — |
| `reliability_weight_i` | Weight on own rate | fraction | [0,1] | ↑ with minutes | Derived | Derived | — | — | + prior_weight = 1 |
| `prior_weight_i` | Weight on prior | fraction | [0,1] | ↑ with m₀ | Derived | Derived | — | — | — |
| `adjusted_rate_i` | Shrunk rate | metric | metric | Same | Derived | Estimated | league×position | NA if inputs NA | — |
| `translation_factor_*` | League→MLS multiplier | unitless | ~0.55–1.15 | Higher keeps more | `league_tiers.yml` | Configured | league_id | Unknown→0.70 | Per league |
| `tf_uncertainty` | Translation uncertainty | fraction | 0–1 | Higher worse | `league_tiers.yml` | Configured | league_id | Unknown→0.25 | Per league |
| `percentile_j` | Reference percentile | points | 0–100 | Higher better | Feature build | Estimated | position / MLS | Never→50 | — |
| `w_j` | Role metric weight | unitless | ≥0 sum~1 | Higher→more influence | `roles.yml` | Configured | role | — | Per role |
| `coverage_i` | Role coverage | fraction | 0–1 | Higher better | Derived | Derived | role metrics | 0 if none | unavailable < **0.50** |
| `RoleFit_i` | Tactical Role Fit | points | 0–100/NA | Higher better | `score_role_fit_with_coverage` | Estimated | role | NA if low coverage | thresholds |
| `Contribution_i` | Near-Term Contribution | points | 0–100/NA | Higher better | contribution heuristic | Estimated | MLS/position | NA if no inputs | fallback_heuristic |
| `minutes_factor_i` | Contrib minutes damper | unitless | [0.7,1] | ↑ with minutes | clip(minutes/2500) | Rule | — | — | 0.7–1.0 |
| `cost_tier` | Pay band | tier | 1–5/NA | Higher=more expensive | MLSPA/ASA | Observed | USD bands | Unknown OK | See About tiers |
| `cost_penalty_i` | Value penalty | points | 20/40/60/80/95 | Higher hurts Value | `component_scores.R` | Rule | — | NA if unknown | **20/40/60/80/95** |
| `Value_i` | Comp.-Adj. Value | points | 0–100/NA | Higher better | `score_compensation_adjusted_value` | Estimated | — | NA if pay unknown | — |
| `Feasibility_i` | Acquisition Feasibility | points | 0–100 | Higher better | `score_feasibility` | Rule | — | Neutral sub-scores | gate **35** |
| `Development_i` | Development Upside | points | 0–100 | Higher better | `score_development` | Estimated | age curve | Drop NA YoY/minutes | age_curves |
| `ModelUncertainty_i` | Model Uncertainty (`score_risk`) | points | 0–100 | **Lower better** | `score_model_uncertainty` | Estimated | — | defaults for tf/coverage | 0.40/0.35/0.25 |
| `λ` | Uncertainty penalty | unitless | 0–1 | Higher→stronger cut | `config.yml` | Configured | — | — | **0.15** |
| `Overall_i` | Overall score | points | 0–100 | Higher better | `club_utility_score` | Policy blend | club context | components→50 | model.yml |
| `NeedScore` | Roster need | points | 0–100 | Higher=more need | `analyze_club_role_needs` | Estimated | clubs×role | incomplete roster | roster_needs.weights |
| `RoleSimilarity` | Incumbent–target similarity | points | 0–100/NA | Higher=more similar | cosine on role pcts | Estimated | role metrics | NA if cov<0.5 | 0.50 floor |
| `UpgradePotential` | Sporting upgrade | points | 0–100 | Higher better | 0.7ΔC+0.3ΔR | Estimated | pair | — | 0.7/0.3 |
| `blend_weight_2026` | YTD share in blend | fraction | [0.15,0.90] | Higher→more YTD | `evaluation_periods.R` | Derived | — | — | m0=700 |

---

## Score stability

Scores use a fixed season × position × role reference population.  
Filters change **who appears** and **filter-relative rank**, not the underlying index (when player data and club context are unchanged).

---

## Validation requirements

See `scripts/run_backtests.R` and `docs/validation/`. Required before strong language:

1. Next-season / 12-month contribution backtests  
2. League-mover translation estimates  
3. Top-k shortlist success vs baselines  
4. Ranking / recommendation stability  

Until then: directional estimates only.
