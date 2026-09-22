#!/usr/bin/env Rscript
# Deploy 2026 MLS Value Index to shinyapps.io
#
# Prerequisites:
#   install.packages("rsconnect")
#   rsconnect::setAccountInfo(name=..., token=..., secret=...)
# Or set env vars SHINYAPPS_ACCOUNT / SHINYAPPS_TOKEN / SHINYAPPS_SECRET.
#
# Usage (from repo root):
#   Rscript scripts/deploy_shinyapps.R

source("R/utilities/load_project.R")
ensure_packages("rsconnect")

acct <- Sys.getenv("SHINYAPPS_ACCOUNT", "")
token <- Sys.getenv("SHINYAPPS_TOKEN", "")
secret <- Sys.getenv("SHINYAPPS_SECRET", "")

if (nzchar(acct) && nzchar(token) && nzchar(secret)) {
  rsconnect::setAccountInfo(name = acct, token = token, secret = secret)
}

accounts <- tryCatch(rsconnect::accounts(), error = function(e) NULL)
if (is.null(accounts) || !nrow(accounts)) {
  stop(
    "No rsconnect account configured. Run rsconnect::setAccountInfo(...) ",
    "or set SHINYAPPS_ACCOUNT / SHINYAPPS_TOKEN / SHINYAPPS_SECRET."
  )
}

# Confirm score files exist locally (gitignored, so must be force-included)
score_paths <- file.path(
  PROJECT_ROOT, "data", "processed",
  c(
    "player_value_scores_blended.csv",
    "player_value_scores.csv",
    "data_provenance.json"
  )
)
missing <- score_paths[!file.exists(score_paths)]
if (length(missing)) {
  stop(
    "Missing processed score files required for deploy:\n  ",
    paste(missing, collapse = "\n  "),
    "\nRun scripts/06_generate_value_index.R first."
  )
}

# Only ship what the live Shiny app sources. listDeploymentFiles() previously
# packed the whole repo (.github, tests, notebooks, pipeline scripts, reports),
# which caused shinyapps.io "Timeout during request" on the daily refresh.
rel_extra <- function(paths) {
  paths <- paths[file.exists(paths)]
  if (!length(paths)) return(character())
  sub(paste0("^", gsub("([.|()\\^{}+$*?]|\\[|\\])", "\\\\\\1", PROJECT_ROOT), "/?"), "", paths)
}

collect_files <- function(rel_paths) {
  out <- character()
  for (p in rel_paths) {
    full <- file.path(PROJECT_ROOT, p)
    if (dir.exists(full)) {
      files <- list.files(full, full.names = TRUE, recursive = TRUE)
      files <- files[!dir.exists(files)]
      out <- c(out, rel_extra(files))
    } else if (file.exists(full)) {
      out <- c(out, p)
    }
  }
  unique(out)
}

app_files <- collect_files(c(
  "app.R",
  "app",
  "config",
  "R/utilities/load_project.R",
  "R/utilities/data_provenance.R",
  "R/models/value_index.R",
  "R/ui",
  "R/exports",
  "data/processed"
))
app_files <- app_files[!grepl("(^|/)(README\\.md|\\.gitkeep)$", app_files)]
if (!length(app_files)) stop("Deploy bundle is empty.")

message("Deploying mls-value-index from ", PROJECT_ROOT)
message("Bundle file count: ", length(app_files))
options(rsconnect.timeout = 3600)
options(rsconnect.http.timeout = 600)

deploy_once <- function() {
  rsconnect::deployApp(
    appDir = PROJECT_ROOT,
    appName = "mls-value-index",
    appFiles = app_files,
    appPrimaryDoc = "app.R",
    launch.browser = FALSE,
    forceUpdate = TRUE,
    lint = FALSE
  )
}

ok <- FALSE
for (attempt in 1:3) {
  ok <- tryCatch({
    deploy_once()
    TRUE
  }, error = function(e) {
    message("Deploy attempt ", attempt, " failed: ", conditionMessage(e))
    if (attempt < 3) Sys.sleep(20 * attempt)
    FALSE
  })
  if (isTRUE(ok)) break
}
if (!isTRUE(ok)) stop("shinyapps.io deploy failed after 3 attempts.")
message("Deploy complete.")
message("URL: https://undervalued-mls.shinyapps.io/mls-value-index/")
