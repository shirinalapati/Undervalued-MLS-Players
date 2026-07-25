# Changelog

## 1.1.0 — 2026-07-25

- Research question locked to position-adjusted on-ball impact (removed “adjusted team impact” claims)
- Value labels require surplus floors; surplus ≤ 0 can never be Undervalued / Strong / Elite
- Below Expected Value by Current Model uses surplus ≤ −15 (no compensation-percentile gate)
- Add sequential Display Rank across official eligible; within-position rank labeled Position Rank
- Standardize visible Goals Added rates to per 96 minutes; expose Model Confidence = 100 − uncertainty
- Dynamic players-evaluated / official-eligible counts shared by About, banner, Excel, and summary JSON
- Compare Players: grouped bars for 0–100 fields + separate Value Surplus chart
- Expand release tests for labels, ranks, counts, G+/96 units, filters, and MLS-only eligibility

## 1.0.0 — 2026-07-24

- Pivot from MLS Recruitment Intelligence to **2026 MLS Value Index**
- Primary score: reliability-adjusted blended total G+/96 positional percentile
- Value Surplus and Undervaluation Score among MLS players with known 2026 compensation
- New Shiny navigation: About, Rankings, Position, Team, Profile, Compare
- Archive recruitment-engine modules under `archive/recruitment_engine/`
- Add data-quality tests and 2025→2026 stability validation
- Excel workbook export for official rankings
