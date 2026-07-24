# ACCURACY_AUDIT.md — MLS Recruitment Intelligence

**Model status:** Transparent heuristic prototype with **hybrid architecture** (`model_spec` v0.3.0)  
**Audit date:** 2026-07-23  
**Product season:** 2026  
**Directive:** Do not present unvalidated indices as precise projections, financial surplus, or confirmed acquisition feasibility. Validation required before strong language.

**Hybrid (see `docs/architecture.md`):** Learn coefficients only when a measurable historical outcome exists; keep club preferences and decision-layer weights configurable; keep recommendation invariants and role metric sets as transparent rules. Do not learn every coefficient.

---

## 0. Executive summary

| Area | Status | Severity |
|------|--------|----------|
| Recommendation labels vs deltas | **Broken** — objective label overrides evidence (e.g. “Lower-cost” when more expensive) | Critical |
| Missing metrics | **Misleading** — NA → displayed/used as 50 creates fake similarity | Critical |
| “Projected MLS” | **Misnamed** — universal attacking blend; unvalidated; double-counts g+/xG | Critical |
| Proxy metrics | **Misrepresented** — pressures/tackles/progressions derived from g+ components, not ASA natives | Critical |
| Financial “Value” | **Overclaimed** — compensation bands ≠ acquisition cost / surplus | High |
| Risk | **Mixed concepts** — model uncertainty + cost + nationality double-counted with feasibility | High |
| Age | **Step cliffs + multi-channel reward** | High |
| Scores within shortlist | **Unstable** — ranks OK; must not rescale scores by filter set | High |
| Roster | **Partial** — salary guide + 2026 minutes; not full official MLS designations | Medium |
| Methodology consistency | **Divergent** — About / docs / code disagree | High |
| Backtesting | **Absent** | Critical for language |

**Validation status for all formulas below:** `unvalidated_heuristic` unless a cited test exists.

---

## 1. Inventory — user-facing fields

### 1.1 Shortlist (`app/app.R`)

| Display | R variable | Formula / source | Assumption | Limitation | Validation | Required tests |
|---------|------------|------------------|------------|------------|------------|----------------|
| `#` | `rank` | `row_number()` after filters | Rank is filter-relative | OK | — | Rank changes with filters; score must not |
| Player | `display_name` | ASA players | Name join correct | — | — | ID stability |
| League | `league_id` | ASA | — | — | — | — |
| Age | `age` | birthdate or league default | Defaults OK if DOB missing | Defaults invent age | unvalidated | DOB coverage |
| Overall | `score_overall` | See §2.1 | Weights meaningful | Double-counts age/cost/style | unvalidated | Stability + invariants |
| Club fit | `score_club_personalization` | 0.45 style + 0.20 age + 0.20 budget + 0.08 domestic + 0.07 pathway | Club YAML priors valid | Public estimate | unvalidated | — |
| Style fit | `score_club_fit` | Club-weighted percentiles | Proxies ≈ style | Proxies collinear | unvalidated | — |
| Age fit | `score_age_fit` | Priority-specific age score | Continuous preferred | Was stepwise / multi-channel | unvalidated | Continuity |
| Budget fit | `score_budget_fit` | cost_tier vs club max | Tier ≈ affordability | Not acquisition cost | unvalidated | — |
| Proj MLS | `score_projected_mls` | Universal 0.35/0.25/0.25/0.15 | Same for all roles | Misnamed; attacking bias | unvalidated | Role-specific + backtest |
| Role fit | `score_role_fit` | Weighted `*_pct`; missing→50 | Coverage adequate | Fake similarity | unvalidated | Coverage gates |
| Value | `score_financial_value` | proj − cost_penalty + 50 | Salary ≈ value | Not surplus | unvalidated | Rename + unknown tier |
| Upside | `score_development` | age bands + yoy + minutes + residual | Age≈upside | Cliffs; double-count | unvalidated | Continuous age |
| Risk | `score_risk` | sample+translation+age+intl+cost+missing | Single risk OK | Mixed concepts; double-count | unvalidated | Split risks |
| Rec | `recommendation` | Pursue/Dev/Monitor/Pass thresholds | Labels actionable | Too strong; not invariant-checked | unvalidated | Cautious labels + tests |
| WhyClub | `why_club` | sprintf ClubFit/AgeFit/BudgetFit/Pathway | — | — | — | — |

### 1.2 Comparison (`roster_comparison.R`)

| Field | Logic | Limitation | Required fix |
|-------|-------|------------|--------------|
| Relationship | **User objective first**, else score heuristics | Lower-cost not verified | Hard invariants |
| Role similarity | Cosine of role metrics centered at 50 | Missing→50 inflates sim | NA + coverage |
| Upgrade / Complement / Fin.eff / Succession | Heuristic blends | Unvalidated | Gate by invariants |
| Metric deltas | Target − incumbent percentiles | 0 delta when both missing | Show N/A |

### 1.3 Roster / needs

| Field | Source | Limitation |
|-------|--------|------------|
| Membership | 2026 MLSPA/ASA salary + 2026 minutes | Not official MLS roster designations (DP/U22/TAM/intl/unavailable) |
| Status | `active_2026` vs `on_books_no_2026_minutes` | Loans still “on books” |
| Need Score | League-percentile gaps | Unvalidated |

---

## 2. Formulas (as implemented before accuracy patch)

### 2.1 Overall (club-conditioned, Shiny default)

```
base = w_proj·Proj + w_role·Role + w_fin·Value + w_feas·Feas + w_dev·Upside
ClubFitComposite = 0.45·Style + 0.20·AgeFit + 0.20·Budget + 0.08·Domestic + 0.07·Pathway
Overall = 0.45·base + 0.55·ClubFitComposite  (+ optional risk multiplier; +tiebreak ≤1.5)
```

Config defaults: proj 0.30, role 0.25, financial 0.20, feasibility 0.15, development 0.10.

**Known issue:** Age, cost, and style enter through multiple channels.

### 2.2 “Projected MLS” (to be renamed)

```
raw = 0.35·pct_npxg + 0.25·pct_xa + 0.25·pct_gplus + 0.15·pct_press
× age_factor(peak 26.5) × minutes_factor
```

**Horizon:** undefined. **Backtest:** none.

### 2.3 Role fit

Weighted mean of role `*_pct` columns from `config/role_weights.yml`; missing → 50.

### 2.4 Recommendation labels (legacy)

| Label | Rule |
|-------|------|
| Pass | feasibility < 30 or none match |
| Pursue | overall≥70 & risk≤45 & feas≥55 |
| Development target | development≥70 & overall≥55 & feas≥50 |
| Monitor | overall≥55 & risk≤65 |

---

## 3. Metric provenance

| Metric | Real ASA? | Implementation | Display requirement |
|--------|-----------|----------------|---------------------|
| npxG, xA, shots, goals, minutes | Yes | ASA xG / xPass | OK |
| goals_added (+ components) | Yes | ASA g+ | Prefer components by role |
| salary / guaranteed compensation | Yes (MLSPA via ASA) | USD; as-of release date | Show as-of |
| pressures_p90 | **No** | `g+_defending·k + c` | Label **proxy** |
| tackles / interceptions | **No** | Same defending field | Label **proxy** |
| progressive_passes / carries | **No** | From g+_passing / g+_dribbling | Label **proxy** |
| turnovers_forced, progressive_runs, ball_retention, transition_involvement | **No** | Aliases of proxies | Label **proxy** or remove |
| aerial_win_pct | **No** | Constant 0.48 | NA / remove |
| crosses_p90 | **No** | Constant by position | NA / remove |
| fouls_won_ratio | **No** | Hardcoded 50 | NA |

---

## 4. Missing-value policy (current → required)

| Current | Required |
|---------|----------|
| Display / use 50 for missing | Store NA; display “Not available” |
| Missing vs missing → Δ0 / high similarity | Do not treat as equal; exclude from cosine |
| Flat missing_metric_risk=20 | Coverage-based confidence penalty |
| Role fit with any coverage | Gate: coverage&lt;50% unavailable; 50–70% max Low; 70–85% max Medium; &gt;85% High eligible |

---

## 5. Data sources & cutoffs (must be separated)

| Source | What | Typical as-of | Notes |
|--------|------|---------------|-------|
| ASA performance | Minutes, xG, xPass, g+ | Pipeline retrieval time | YTD incomplete |
| MLSPA/ASA salaries | Guaranteed compensation | e.g. 2026-04-16 release | Not budget charge |
| Roster | Salary guide + 2026 minutes | Salary release + YTD | Not official MLS designations |
| Transactions | Not systematically ingested | — | Required for accurate roster |
| Club profiles YAML | Style/budget priors | Static estimate | Not club strategy |

Single banner cutoff is **misleading** — must show separate dates.

---

## 6. Cost / salary

| Concept | Current | Required |
|---------|---------|----------|
| Guaranteed compensation | Used as “salary” | Keep; label + as-of |
| Salary budget charge | Not modeled | Unknown |
| Acquisition cost (fee/GAM/trade) | Not modeled | Unknown; do not fabricate |
| cost_tier | Always assigned (placeholders for missing) | Unknown if no compensation |
| Financial Value | “surplus” language | Rename Compensation-Adjusted Value Index |

Placeholders (MLS $350k / USLC $70k / MLSNP $45k) **must not** invent tiers for unknown non-MLS pay.

---

## 7. International / domestic

```
is_domestic = grepl("USA|CAN|…", nationality)
```

**Invalid:** nationality ≠ MLS international-roster status. Prefer official roster status; else **Unknown**. Do not auto-penalize on nationality alone.

---

## 8. Recommendation invariants (required)

### Lower-cost alternative
- `target_cost < incumbent_cost` (known costs only)
- AND `target_contribution >= incumbent_contribution - tolerance`
- Missing financial data → **cannot** trigger this label

### Immediate upgrade
- `target_contribution >= incumbent + min_margin`
- AND `target_role_fit >= threshold` (and coverage OK)
- AND confidence ≥ medium

### Developmental successor
- Younger
- Upside ≥ threshold
- Acceptable role fit / coverage
- Plausible acquisition pathway

### Else
- **No clear recruitment advantage identified**

Tests must enforce written label ↔ deltas.

---

## 9. League comparison order (required)

1. Raw rate  
2. Reliability shrinkage  
3. League→MLS translation (**assumed** tier factors until estimated)  
4. Context adjustment  
5. MLS position/role reference percentiles  

Do **not** treat USL percentile as MLS-equivalent percentile.

---

## 10. Score stability (required)

- Compute scores vs fixed `season × position × role × eligible leagues` reference.
- Filters change **rank among filters** and who appears; not the player’s index.
- Display: Score, Rank among filters, Reference percentile.

---

## 11. Cautious language (until backtests)

| Avoid | Prefer |
|-------|--------|
| Projected MLS / projection | Estimated Near-Term Contribution Index |
| Financial surplus / Value | Compensation-Adjusted Value Index |
| Pursue / Highly attainable | Priority Review / Low Priority |
| Confirmed feasibility | Estimated acquisition plausibility |
| Learned translation | Assumed league-strength adjustment |

Legacy Rec → map toward: **Priority Review, Monitor, Development Watch, Low Priority**.

---

## 12. Validation plan (required before strong claims)

1. **Time-split contribution:** cutoff → next-season/12m MLS contribution; MAE/RMSE/rank corr by position.  
2. **League movers:** USLC/MLSNP→MLS; pre/post change with CIs.  
3. **Shortlist success:** historical snapshot top-k vs later useful contributors; vs baselines (g+/90, xG+xA, age, salary, minutes).  
4. **Stability:** bootstrap minutes; top-10 retention; recommendation flip rate.

Until then: all scores = **unvalidated_heuristic**.

---

## 13. Source-of-truth config (required)

Single versioned artifact, e.g. `config/model_spec.yml`:

```yaml
model_version: "0.2.0"
effective_date: "2026-07-23"
labels: ...
weights: ...
invariants: ...
coverage_thresholds: ...
contribution_by_role: ...
```

About, Methodology, Excel exports, README **must render from this file** — no hand-copied formulas.

---

## 14. Existing tests

`tests/test_scoring_framework.R` — ranges/plumbing only. **Insufficient.**

Required new tests (minimum):
- More expensive target ≠ Lower-cost  
- Weaker contribution ≠ Immediate upgrade  
- Missing finance ≠ financial recommendation  
- Narrative/label agrees with deltas  
- Missing vs missing ≠ equal / Δ0 plotted  
- Coverage &lt;50% → role fit unavailable  
- Filter change does not change fixed-reference score  

---

## 15. Implementation priority (this remediation)

1. Recommendation invariants + tests  
2. NA / coverage / proxy labeling  
3. Rename contribution & value; cautious Rec labels  
4. Role-specific contribution index (g+ components)  
5. Risk split; continuous age; reduce double-count  
6. Fixed-reference scores + filter ranks  
7. Separate source cutoffs; roster designation fields where available  
8. `model_spec.yml` SoT + About/methodology sync  
9. Backtest harness (even if initially empty results)

**No new visual features until critical accuracy issues above are resolved.**
