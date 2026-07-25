# Data sources

| Source | Use | Notes |
|--------|-----|-------|
| American Soccer Analysis (`itscalledsoccer`) | Goals Added totals/components, minutes, identities | 2025 full + 2026 YTD |
| MLSPA via ASA | 2026 guaranteed compensation | Annualized USD |
| Internal team map | Club names | From ASA team endpoint / clean tables |

Cutoff dates are stored in `data/processed/data_provenance.json` and shown in the app banner.

Production rankings never silently fall back to synthetic demo players while `project.mode: live`.
