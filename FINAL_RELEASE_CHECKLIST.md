# Final Release Checklist — 2026 MLS Value Index

## Product identity
- [x] App renamed to 2026 MLS Value Index
- [x] README / docs describe Value Index (not recruitment engine)
- [x] Recruitment features archived under `archive/recruitment_engine/`
- [x] Backup branch `archive/recruitment-engine-backup`

## Data
- [x] Production players are real MLS only
- [x] No MLSNP/USL in official rankings
- [x] Every official eligible player has known 2026 compensation
- [x] All 30 current MLS clubs appear in official rankings
- [x] Data cutoff dates visible in app banner
- [x] Live mode refuses synthetic fallback

## Scoring
- [x] Sporting Impact = positional percentile of adjusted blended total G+/96
- [x] Components explanatory only (not primary re-weights)
- [x] Value Surplus = Impact − Compensation Percentile
- [x] Undervaluation Score = positional surplus percentile among eligible
- [x] Impact floor prevents weak cheap “Undervalued” labels
- [x] Surplus ≤ 0 never labeled Undervalued / Strong / Elite
- [x] Display Rank sequential; Position Rank resets by position
- [x] Missing metrics remain missing
- [x] Filters do not recompute fixed player scores
- [x] Visible G+ rates use per 96 minutes

## Application
- [x] About & Methodology
- [x] MLS Value Rankings (+ missing-compensation table)
- [x] Position Rankings
- [x] Team Value
- [x] Player Profile
- [x] Compare Players (grouped bars + separate Value Surplus)
- [x] Excel export

## Quality
- [x] `tests/test_value_index.R` passes
- [x] Historical stability validation published
- [x] Model version `1.1.0` in config

## Deployment
- [ ] Deployed to hosted Shiny target (run when credentials available)
- [x] Local launch path documented (`docs/deployment.md`)
- [x] GitHub Actions daily refresh workflow present
- [x] Reproducible pipeline via `scripts/run_pipeline.R`

## Remaining optional
- [ ] PowerPoint deck
- [ ] One-page PDF player report
- [ ] Screenshot assets in README
