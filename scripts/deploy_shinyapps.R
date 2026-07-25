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

# Default listDeploymentFiles() skips .gitignore paths (including data/processed).
base_files <- rsconnect::listDeploymentFiles(PROJECT_ROOT)
rel_extra <- function(paths) {
  paths <- paths[file.exists(paths)]
  if (!length(paths)) return(character())
  # paths relative to PROJECT_ROOT
  sub(paste0("^", gsub("([.|()\\^{}+$*?]|\\[|\\])", "\\\\\\1", PROJECT_ROOT), "/?"), "", paths)
}

extra <- c(
  rel_extra(list.files(file.path(PROJECT_ROOT, "data", "processed"), full.names = TRUE, recursive = TRUE)),
  rel_extra(list.files(file.path(PROJECT_ROOT, "data", "external", "demo"), full.names = TRUE, recursive = TRUE))
)

# Keep the bundle lean; skip archive, validation HTML, and invalid renv.lock stub
skip_re <- "^(archive/|reports/_output/|renv\\.lock$|renv/)"
app_files <- unique(c(base_files, extra))
app_files <- app_files[!grepl(skip_re, app_files)]
# Prefer root app.R as the shiny entrypoint
app_files <- unique(c("app.R", app_files))

message("Deploying mls-value-index from ", PROJECT_ROOT)
message("Bundle file count: ", length(app_files))
rsconnect::deployApp(
  appDir = PROJECT_ROOT,
  appName = "mls-value-index",
  appFiles = app_files,
  appPrimaryDoc = "app.R",
  launch.browser = FALSE,
  forceUpdate = TRUE
)
message("Deploy complete.")
message("URL: https://undervalued-mls.shinyapps.io/mls-value-index/")
