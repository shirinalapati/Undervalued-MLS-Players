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

message("Deploying mls-value-index from ", PROJECT_ROOT)
rsconnect::deployApp(
  appDir = PROJECT_ROOT,
  appName = "mls-value-index",
  launch.browser = FALSE,
  forceUpdate = TRUE
)
message("Deploy complete.")
