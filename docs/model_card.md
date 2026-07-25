# Model Card — 2026 MLS Value Index

## Model details

| Field | Value |
|-------|-------|
| Name | 2026 MLS Value Index |
| Version | 1.1.0 |
| Type | Descriptive compensation-efficiency ranking |
| Primary sporting measure | Reliability-adjusted blended total G+/96 positional percentile |
| Research question | Which current MLS players provide the most compensation-efficient position-adjusted on-ball impact? |
| Owner use case | Portfolio analytics / scouting research aid |

## Intended use

Identify current MLS players whose public, position-adjusted on-ball impact (Goals Added) exceeds their guaranteed-compensation standing.

## Out of scope

Adjusted team impact, off-ball modeling, plus-minus, transfer fees, trade value, GAM, Salary Budget Charge, contract length, availability, acquisition recommendations, multi-club private strategy packs.

## Training data

Not a fitted predictive model. Uses ASA Goals Added and MLSPA guaranteed compensation. No learned coefficients in the primary score.

## Metrics

| Name | Definition |
|------|------------|
| Sporting Impact | Positional percentile of adjusted blended G+/96 |
| Compensation Percentile | Positional percentile of 2026 guaranteed pay |
| Value Surplus | Impact − compensation percentile |
| Undervaluation Score | Positional percentile of Value Surplus among eligible players |
| Display Rank | Sequential rank across all official eligible players |
| Position Rank | Rank within position among official eligible players |
| Model Confidence | 100 − Model Uncertainty |

## Value labels (release rules)

No player with Value Surplus ≤ 0 can be labeled Undervalued, Strong Value, or Elite Value.

| Label | Requirements |
|-------|----------------|
| Elite Value | Surplus ≥ 25, Undervaluation Score ≥ 90, Impact ≥ 70, Medium/High confidence |
| Strong Value | Surplus ≥ 15, Undervaluation Score ≥ 75, Impact ≥ 60, Medium/High confidence |
| Undervalued | Surplus ≥ 5, Undervaluation Score ≥ 60, Impact ≥ 55 |
| Fair Value | Surplus in (−15, 5), or fails higher-label floors |
| Below Expected Value by Current Model | Surplus ≤ −15 |

## Ethical / product considerations

Do not treat “Below Expected Value by Current Model” as an objective employment judgment. Public metrics omit many coaching and off-ball contributions. Goals Added measures measurable on-ball impact only.

## Validation

See `docs/validation.md` and `reports/_output/model_validation.html`.
