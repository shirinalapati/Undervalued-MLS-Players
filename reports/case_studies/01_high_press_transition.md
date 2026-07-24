# Case Study 1: High-pressing transition club
#
# Public-data estimated profile (example): New York Red Bulls–like pressing identity.
# This is NOT an official club document.

## Objective

Identify attainable forwards and wide attackers who fit an aggressive press and
quick vertical transition game, with preference for development upside.

## Club profile priors used

- High `pressing_weight` / `transition_weight`
- Elevated `development_priority`
- Medium budget → prefer cost tiers 1–2
- Age preference roughly 18–25 for primary targets

## Role focus

- Primary: Pressing striker
- Secondary: Transition winger
- Complementary: Ball-winning midfielder

## Filters applied

- Leagues: MLS, MLS NEXT Pro, USL Championship
- Max cost tier: Moderate
- Risk tolerance: Medium
- Priority: Development prospect

## How to reproduce

1. Run pipeline (`scripts/00`–`06`) in demo or live mode.
2. Open Shiny app → select **New York Red Bulls** → **Pressing Striker**.
3. Or inspect `data/processed/rankings_case_studies.csv` filtered to `club_id == rbnY`.

## Interpretation standard

Shortlists emphasize **pressing/transition proxies**, **feasibility**, and **upside**.
A high-usage MLS Designated Player who does not press should not outrank a cheaper
USL forward with elite pressing engagement — even if raw finishing is higher.

## Deliverable for portfolio

- Excel shortlist (10–15)
- One Pursue player report
- Explicit limitations: translation uncertainty, missing true pressure events, no medicals
