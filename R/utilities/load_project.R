# Load project configuration and common utilities.
# Sourced by scripts via: source("R/utilities/load_project.R")

suppressPackageStartupMessages({
  if (!requireNamespace("here", quietly = TRUE)) {
    .proj_root <- normalizePath(getwd())
  } else {
    .proj_root <- tryCatch(here::here(), error = function(e) normalizePath(getwd()))
  }
})

PROJECT_ROOT <- .proj_root

ensure_packages <- function(pkgs) {
  missing <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing)) {
    stop(
      "Missing R packages: ", paste(missing, collapse = ", "),
      "\nInstall with renv::restore() or install.packages().",
      call. = FALSE
    )
  }
}

load_config <- function(path = file.path(PROJECT_ROOT, "config", "config.yml")) {
  ensure_packages("yaml")
  cfg <- yaml::read_yaml(path)
  if (!is.null(cfg$default)) cfg <- cfg$default
  cfg$paths <- lapply(cfg$paths, function(p) {
    if (grepl("^/", p) || grepl("^[A-Za-z]:", p)) p else file.path(PROJECT_ROOT, p)
  })
  cfg
}

load_yaml <- function(rel_path) {
  ensure_packages("yaml")
  yaml::read_yaml(file.path(PROJECT_ROOT, rel_path))
}

dir_create_safe <- function(...) {
  path <- file.path(...)
  if (!dir.exists(path)) dir.create(path, recursive = TRUE, showWarnings = FALSE)
  path
}

write_log <- function(...) {
  msg <- paste0("[", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "] ", paste0(..., collapse = ""))
  message(msg)
  invisible(msg)
}

normalize_player_name <- function(x) {
  x <- tolower(trimws(as.character(x)))
  x <- iconv(x, from = "", to = "ASCII//TRANSLIT")
  x <- gsub("[^a-z0-9 ]", "", x)
  x <- gsub("\\s+", " ", x)
  x
}

scale_0_100 <- function(x, na_fill = 50) {
  x <- as.numeric(x)
  if (all(is.na(x))) {
    if (is.na(na_fill)) return(rep(NA_real_, length(x)))
    return(rep(na_fill, length(x)))
  }
  rng <- range(x, na.rm = TRUE)
  if (diff(rng) < 1e-9) {
    return(ifelse(is.na(x), na_fill, 50))
  }
  out <- 100 * (x - rng[1]) / diff(rng)
  if (is.na(na_fill)) {
    # preserve missingness — do not invent neutral performance
  } else {
    out[is.na(out)] <- na_fill
  }
  pmin(pmax(out, 0), 100)
}

percentile_rank <- function(x) {
  x <- as.numeric(x)
  out <- rep(NA_real_, length(x))
  ok <- !is.na(x)
  if (!any(ok)) return(out)
  out[ok] <- 100 * rank(x[ok], ties.method = "average") / sum(ok)
  out
}

empirical_bayes_shrink <- function(rate, minutes, prior_rate, m0 = 600) {
  w <- minutes / (minutes + m0)
  w * rate + (1 - w) * prior_rate
}

clip <- function(x, lo, hi) pmin(pmax(x, lo), hi)

recommendation_label <- function(overall, risk, development, feasibility) {
  # Cautious labels from recommendation_rules / thresholds (loaded if available).
  thr <- tryCatch(load_thresholds(), error = function(e) list())
  rt <- thr$recommendation_score_thresholds %||% list()
  pr <- rt$priority_review %||% list(min_overall = 70, max_model_uncertainty = 45, min_feasibility = 55)
  dw <- rt$development_watch %||% list(min_development = 70, min_overall = 55, min_feasibility = 50)
  mon <- rt$monitor %||% list(min_overall = 55, max_model_uncertainty = 65)
  dplyr::case_when(
    feasibility < (rt$low_priority$max_feasibility_for_auto_low %||% 30) ~ "Low Priority",
    overall >= (pr$min_overall %||% 70) &
      risk <= (pr$max_model_uncertainty %||% 45) &
      feasibility >= (pr$min_feasibility %||% 55) ~ "Priority Review",
    development >= (dw$min_development %||% 70) &
      overall >= (dw$min_overall %||% 55) &
      feasibility >= (dw$min_feasibility %||% 50) ~ "Development Watch",
    overall >= (mon$min_overall %||% 55) &
      risk <= (mon$max_model_uncertainty %||% 65) ~ "Monitor",
    TRUE ~ "Low Priority"
  )
}
