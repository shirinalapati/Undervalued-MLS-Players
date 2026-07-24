# DATA_SOURCE_AUDIT.md

**Date:** 2026-07-23  
**Product season:** 2026  

This audit separates **observed facts**, **model estimates**, **assumed adjustments**, and **missing / unknown**.

---

## Source inventory

| Domain | Primary source | As-of / cutoff | Status | Notes |
| --- | --- | --- | --- | --- |
| Performance (MLS, USLC, MLSNP) | ASA API via `itscalledsoccer` | Retrieval timestamp (live cache) | Partially complete | 2024–2026 seasons collected; **g+ rates currently zero in processed tables** |
| Guaranteed compensation | MLSPA guide via ASA salaries | Config `salary_as_of` / guide release (e.g. 2026-04-16) | Partially complete | USD; not Salary Budget Charge |
| Roster membership | MLSPA/ASA salary guide + 2026 minutes | Salary as-of + YTD | Incorrect vs product definition | Must become official roster snapshot + transactions |
| Roster designations (DP/U22/TAM/intl/unavailable) | Official MLS roster profiles | e.g. Feb 2026 snapshot when ingested | Not started / Blocked | Do not infer from nationality or minutes |
| Transactions | MLS transaction feeds | Not systematically ingested | Not started | Required for accurate roster backbone |
| League strength | `config/league_tiers.yml` | Static | Assumed | Label: assumed league-strength adjustments |
| Club profiles | `config/club_profiles.yml` | Public estimates | Configurable priors | User-overridable; not internal strategy |
| Demo / synthetic | `data/external/demo` | N/A | Dev only | Must never silently power production app |

---

## Critical defects

1. **Provenance mismatch:** `data/processed/data_provenance.json` can report `is_synthetic: true` while `player_features_*.csv` / scores are ASA live. App trusts provenance → false synthetic banner and export block.  
2. **Goals Added empty:** `goals_added_p90` = 0 across interim/processed → contribution and g+-derived proxies invalid.  
3. **Proxy metrics:** pressures/tackles/progressions derived from g+ components or constants — must be labeled proxies; ASA summary does not document native `pressures_p90`.  
4. **Single cutoff UX:** Historically one “data cutoff” implied all sources current; must show separate dates.  
5. **International status:** Must not equal nationality; store Unknown until official roster status exists.

---

## Field handling policy

| Field | If missing |
| --- | --- |
| Performance metric | NA / Not available; coverage ↓; no neutral 50 |
| Guaranteed compensation | Unknown; no fabricated tier for non-MLS |
| Acquisition cost | Unknown / estimated tier only if model fitted + labeled estimate |
| International roster status | Unknown |
| Contract / option | Unknown |
| Roster designation | Unknown |

---

## Required cutoff display (production)

```
Performance data through: YYYY-MM-DD
MLSPA compensation as of: YYYY-MM-DD
Official roster profile as of: YYYY-MM-DD
Transactions incorporated through: YYYY-MM-DD
Model training data through: YYYY-MM-DD
Model version: X.Y.Z
```

---

## Pipeline files

| Step | Script |
| --- | --- |
| Collect | `scripts/01_collect_data.R` |
| Clean | `scripts/02_clean_data.R` |
| DB load | `scripts/03_load_database.R` |
| Features | `scripts/04_build_features.R` |
| Scores | `scripts/05_train_models.R` |
| Learned fit | `scripts/08_train_learned_models.R` |
| Rankings/exports | `scripts/06_generate_rankings.R` |
| Daily refresh | `scripts/refresh_daily.R` |

---

## Acceptance for Phase 2

- [ ] Provenance matches loaded data mode  
- [ ] g+ (and documented natives) non-degenerate for MLS seasons with ASA coverage  
- [ ] Separate cutoffs in provenance + UI + exports  
- [ ] No production synthetic players  
- [ ] Roster backbone from official snapshot + transactions (or explicitly disclosed interim limitation)
