# Validation — 2026 MLS Value Index

## Claim boundary

The product identifies **current** players whose public performance exceeds compensation standing. It does **not** claim guaranteed future breakouts unless stronger outcome validation is published.

## Stability check (2025 → 2026 YTD)

Script: `scripts/09_run_validation.R`  
Artifacts:

- `docs/validation/value_index_stability.json`
- `reports/_output/model_validation.html`

Measures:

- Spearman correlation of Sporting Impact (2025 full vs 2026 YTD)
- Spearman correlation of Value Surplus
- Share of 2025 position top-25 with 2026 Impact ≥ 50
- Simple baselines (prior impact; compensation-standing proxy)

## Baselines (conceptual)

1. Goals Added / Sporting Impact alone  
2. Compensation percentile alone  
3. Previous-season minutes  
4. Raw performance − compensation percentile  
5. Position median  

## Interpretation

Use stability results to understand persistence of public-data signals. Do not market the Index as a salary-growth or minutes-guarantee model without dedicated outcome studies.
