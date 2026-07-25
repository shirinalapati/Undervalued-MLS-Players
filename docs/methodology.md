# Methodology — 2026 MLS Value Index

## Research question

Which current MLS players provide the most compensation-efficient position-adjusted on-ball impact?

The 2026 MLS Value Index identifies players whose position-adjusted on-ball impact ranks meaningfully higher than their compensation among MLS players at the same position.

The production model measures position-adjusted on-ball impact through Goals Added only. It does not estimate adjusted team impact, off-ball contribution, or plus-minus.

This is a current-value analysis. It does not attempt to estimate transfer fees, trade value, future contracts, or whether a player is available to move.

## How the index works

1. Estimate each player’s position-adjusted sporting impact.
2. Compare the player’s compensation with others at the same position.
3. Calculate the gap between performance and compensation.
4. Rank that gap against eligible positional peers.

### 1. Position-Adjusted Sporting Impact

Primary measure: reliability-adjusted, blended total Goals Added per 96 minutes.

Components (Shooting, Passing, Receiving, Dribbling, Interrupting, Fouling) explain profiles on Player Profile pages. They are not reweighted into the primary score because total Goals Added already combines them. All visible Goals Added rates use per 96 minutes.

```
Adjusted G+/96 = reliability-adjusted player G+/96
Sporting Impact = position-group percentile of Adjusted G+/96
```

### 2. Sample-size adjustment

```
Reliability Weight = Player Minutes / (Player Minutes + 600)
Prior Weight = 1 − Reliability Weight
Adjusted Rate = Reliability Weight × Observed Rate + Prior Weight × Position Prior
```

The 600-minute prior is a configured assumption and has not yet been learned separately for each metric.

### 3. 2025+2026 season to date

Default evaluation combines 2026 season-to-date with 2025 full-season performance.

```
w2026 = clamp(2026 Minutes / (2026 Minutes + 700), 0.15, 0.90)
Blended G+/96 = w2026 × Adjusted 2026 G+/96 + (1 − w2026) × Adjusted 2025 G+/96
```

Players without valid 2025 data use reliability-adjusted 2026 only and receive lower Data Confidence.

### 4. Compensation Percentile

```
Compensation Percentile = position-group percentile of guaranteed compensation
```

### 5. Value Surplus

```
Value Surplus = Sporting Impact − Compensation Percentile
```

A percentile-point difference, not a dollar amount.

### 6. Undervaluation Score

```
Undervaluation Score = position-group percentile of Value Surplus
```

**Display Rank** is the sequential order of Undervaluation Score across all official eligible players. **Position Rank** is the same ordering within each position group.

## Official ranking eligibility

- Currently associated with an MLS first-team club
- Valid position group
- Known 2026 guaranteed compensation
- Available Sporting Impact score
- At least 450 minutes during the 2026 season
- No synthetic or demonstration data

Players evaluated and official eligible counts are generated dynamically from production score files.

## Value labels

No player with Value Surplus ≤ 0 can be labeled Undervalued, Strong Value, or Elite Value.

| Label | Requirements |
|-------|----------------|
| Elite Value | Surplus ≥ 25, Undervaluation Score ≥ 90, Impact ≥ 70, Medium/High confidence |
| Strong Value | Surplus ≥ 15, Undervaluation Score ≥ 75, Impact ≥ 60, Medium/High confidence |
| Undervalued | Surplus ≥ 5, Undervaluation Score ≥ 60, Impact ≥ 55 |
| Fair Value | Surplus in (−15, 5), or fails an impact/confidence floor for a higher label |
| Below Expected Value by Current Model | Surplus ≤ −15 |
| Small-Sample Watchlist | Interesting signals but below official minutes threshold |
| Insufficient Evidence | Missing pay, impact, position, or eligibility |

## Data Confidence and Model Confidence

- **Data Confidence** — High / Medium / Low / Insufficient based on minutes, 2025/2026 availability, position certainty, performance completeness, and compensation availability. It measures confidence in the evaluation, not player quality.
- **Model Confidence** — `100 − Model Uncertainty`. Higher means more stable statistical evidence.

## Goalkeepers

Goalkeepers are excluded because available public metrics do not support a comparable goalkeeper valuation model.

## Limitations and validation

Not a model of transfer fees, trade value, GAM, Salary Budget Charge, acquisition cost, contract length, future salary, or availability. Primarily a descriptive current-value ranking with historical stability checks — not a guaranteed breakout forecast.

See also `docs/validation.md` and the in-app About page.
