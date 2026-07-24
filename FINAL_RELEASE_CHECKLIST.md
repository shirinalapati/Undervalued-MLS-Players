# FINAL_RELEASE_CHECKLIST.md

Living checklist. **Do not declare the project finished until every item is checked with evidence.**

**Last reviewed:** 2026-07-23  

---

## A. Clubs and production data

- [ ] All configured MLS clubs load in the app  
- [ ] All production player names are real  
- [ ] No synthetic production records remain in live mode  
- [ ] Provenance `is_synthetic` matches actual data mode  
- [ ] Separate cutoffs shown: performance, compensation, roster, transactions, model training  

## B. Methodology and consistency

- [ ] Every score has a documented definition in versioned config  
- [ ] About / Methodology / README / Excel / reports render from the same config  
- [ ] No duplicated hard-coded formulas in the app  
- [ ] Observed vs estimated vs assumed vs unknown clearly distinguished  
- [ ] Missing values are not fabricated (no neutral-50 display)  

## C. Models and scores

- [ ] Forecast horizon defined and used consistently  
- [ ] Sporting contribution excludes salary/feasibility/cost  
- [ ] Fixed-reference scores; filters change rank only  
- [ ] Role-fit coverage gates enforced  
- [ ] League translation order: shrink → translate → MLS reference  
- [ ] Age affects development outlook, not current performance inflation  

## D. Recommendations

- [ ] All recommendation invariants pass automated tests  
- [ ] No contradictory labels (e.g. lower-cost when more expensive)  
- [ ] Cautious labels unless validation supports stronger language  
- [ ] Recommendation text agrees with deltas  

## E. Validation

- [ ] Historical backtests published (`docs/validation.md`, model card, HTML report)  
- [ ] Baselines compared  
- [ ] In-app validation summary visible  

## F. Exports

- [ ] Excel workbook: all required sheets, metadata, no synthetic prod content  
- [ ] Word/PDF one-pager  
- [ ] PowerPoint briefing  
- [ ] Exports match live app calculations  

## G. Tests and reproducibility

- [ ] Full automated test suite green (data, model, recommendation, export, app)  
- [ ] One documented command rebuilds DB → data → models → scores → tests → reports → app  
- [ ] `renv` restore works from clean environment  

## H. Deployment

- [ ] Deployed app uses production data  
- [ ] Model version + cutoffs visible  
- [ ] Credentials not exposed  
- [ ] Refresh failures handled; **no silent synthetic fallback**  
- [ ] Unavailable information disclosed  

## I. Documentation

- [ ] README matches reality (no unimplemented features claimed complete)  
- [ ] architecture, data_sources, data_dictionary, methodology, validation, limitations, model_card, deployment, maintenance, CHANGELOG, CONTRIBUTING  

---

## Sign-off

| Role | Name | Date | Evidence link |
| --- | --- | --- | --- |
| Author | | | |
| Reviewer | | | |

**Release decision:** ☐ Not ready · ☐ Limited portfolio demo · ☐ Production portfolio release  
