# Deployment — 2026 MLS Value Index

## Continuous in-season updates

Performance data is refreshed on a schedule so the 2026 season-to-date rankings stay current.

| Piece | Behavior |
|-------|----------|
| Banner note | `Performance data through: YYYY-MM-DD (updates every day)` |
| Config | `deployment.refresh_cadence_label` in `config/config.yml` |
| Refresh script | `scripts/refresh_daily.R` (force-refetch ASA/MLSPA → rebuild Value Index) |
| Local/server app | `deployment.auto_reload: true` — hot-reloads processed CSVs about every 60s |
| GitHub Actions | `.github/workflows/daily_refresh.yml` — daily rebuild + optional shinyapps redeploy |

### Local / VPS with persistent disk

```bash
# One-time
Rscript scripts/install_dependencies.R
Rscript scripts/run_pipeline.R --force-refresh
Rscript -e "shiny::runApp('app', host='0.0.0.0', port=7788)"

# Cron — refresh data every day (app picks it up without restart)
15 6 * * * cd /path/to/mls-recruitment-intelligence && Rscript scripts/refresh_daily.R >> logs/refresh.log 2>&1
```

### shinyapps.io

shinyapps.io cannot run your daily cron inside the app. Use GitHub Actions:

1. Push this repo to GitHub.
2. Add repository secrets:
   - `SHINYAPPS_ACCOUNT`
   - `SHINYAPPS_TOKEN`
   - `SHINYAPPS_SECRET`
3. Enable Actions. The daily workflow refreshes data, commits processed files, and redeploys when secrets are present.
4. Manual first deploy (local):

```bash
Rscript scripts/deploy_shinyapps.R
```

Or in R:

```r
rsconnect::deployApp(appDir = ".", appName = "mls-value-index", forceUpdate = TRUE)
```

Before deploy, confirm `config/value_index.yml` model version, regenerate scores
(`Rscript scripts/06_generate_value_index.R`), and run `Rscript tests/test_value_index.R`.

### Environment checks

- Live mode refuses synthetic data
- Banner shows performance cutoff + `(updates every day)` + MLSPA as-of date
- No official-roster / transactions placeholders in the banner
- Excel download works after deploy

### Cadence wording

To change the parenthetical (e.g. twice daily), edit:

```yaml
deployment:
  refresh_cadence_label: "every day"   # or "twice daily", "every 12 hours"
```

Keep the scheduled job aligned with that label.
