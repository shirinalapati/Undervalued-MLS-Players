# Artifact I/O for hybrid learned models — load fitted objects or fall back cleanly.

hybrid_artifact_dir <- function(spec = NULL, cfg = NULL) {
  spec <- spec %||% tryCatch(load_model_spec(), error = function(e) NULL)
  rel <- spec$hybrid$artifact_dir %||% "data/processed/models"
  if (!is.null(cfg) && !is.null(cfg$paths$processed)) {
    return(file.path(cfg$paths$processed, "models"))
  }
  file.path(PROJECT_ROOT, rel)
}

ensure_artifact_dir <- function(cfg = NULL, spec = NULL) {
  dir_create_safe(hybrid_artifact_dir(spec, cfg))
}

artifact_path <- function(filename, cfg = NULL, spec = NULL) {
  file.path(hybrid_artifact_dir(spec, cfg), filename)
}

#' Load a learned model artifact if present; otherwise NULL.
load_learned_artifact <- function(name, cfg = NULL, spec = NULL) {
  spec <- spec %||% tryCatch(load_model_spec(), error = function(e) NULL)
  meta <- spec$hybrid$learned[[name]]
  if (is.null(meta)) return(NULL)
  path <- artifact_path(meta$artifact %||% paste0(name, ".rds"), cfg, spec)
  if (!file.exists(path)) return(NULL)
  if (grepl("\\.json$", path, ignore.case = TRUE)) {
    ensure_packages("jsonlite")
    return(jsonlite::fromJSON(path, simplifyVector = TRUE))
  }
  readRDS(path)
}

save_learned_artifact <- function(obj, name, cfg = NULL, spec = NULL) {
  spec <- spec %||% load_model_spec()
  meta <- spec$hybrid$learned[[name]]
  ensure_artifact_dir(cfg, spec)
  path <- artifact_path(meta$artifact %||% paste0(name, ".rds"), cfg, spec)
  if (grepl("\\.json$", path, ignore.case = TRUE)) {
    ensure_packages("jsonlite")
    jsonlite::write_json(obj, path, pretty = TRUE, auto_unbox = TRUE)
  } else {
    saveRDS(obj, path)
  }
  # Sidecar status for provenance
  status_path <- paste0(tools::file_path_sans_ext(path), "_status.json")
  ensure_packages("jsonlite")
  jsonlite::write_json(
    list(
      name = name,
      path = path,
      saved_at = as.character(Sys.time()),
      model_version = spec$model_version,
      coefficient_type = "learned"
    ),
    status_path,
    pretty = TRUE,
    auto_unbox = TRUE
  )
  invisible(path)
}

learned_status <- function(name, spec = NULL, cfg = NULL) {
  spec <- spec %||% tryCatch(load_model_spec(), error = function(e) NULL)
  meta <- spec$hybrid$learned[[name]]
  if (is.null(meta)) return(list(status = "unknown", fitted = FALSE))
  art <- load_learned_artifact(name, cfg, spec)
  list(
    status = meta$status %||% "unknown",
    fitted = !is.null(art),
    artifact = meta$artifact,
    label = meta$fallback_label %||% meta$label %||% name
  )
}

hybrid_status_summary <- function(spec = NULL, cfg = NULL) {
  spec <- spec %||% load_model_spec()
  names_learned <- names(spec$hybrid$learned %||% list())
  lapply(names_learned, function(nm) {
    st <- learned_status(nm, spec, cfg)
    c(list(name = nm), st)
  })
}

`%||%` <- function(a, b) if (!is.null(a)) a else b
