# Model 1 — Sporting contribution (LEARNED when historical outcomes exist).
# Preferred: ridge / elastic net. Heuristic role blends are fallback only.

CONTRIBUTION_FEATURE_CANDIDATES <- c(
  "pct_proj_npxg", "pct_proj_xa", "pct_proj_gplus", "pct_proj_press",
  "pct_prog_pass", "pct_prog_carry", "pct_tackles", "pct_intercept",
  "pct_shots", "age", "minutes", "tf_uncertainty"
)

#' Build a panel with next-season outcome for contribution learning.
#' Requires multi-season player rows with asa_player_id + season_year + goals_added_p90.
build_contribution_training_panel <- function(player_seasons, outcome_col = "goals_added_p90") {
  ensure_packages("dplyr")
  df <- player_seasons
  if (!all(c("asa_player_id", "season_year", outcome_col) %in% names(df))) {
    return(tibble::tibble())
  }
  df <- df |>
    dplyr::arrange(asa_player_id, season_year) |>
    dplyr::group_by(asa_player_id) |>
    dplyr::mutate(
      next_season = season_year + 1L,
      outcome_next = dplyr::lead(.data[[outcome_col]]),
      minutes_next = dplyr::lead(minutes)
    ) |>
    dplyr::ungroup() |>
    dplyr::filter(is.finite(outcome_next), is.finite(.data[[outcome_col]]))
  df
}

assign_time_splits <- function(panel, spec = NULL) {
  spec <- spec %||% load_model_spec()
  ts <- spec$hybrid$time_split
  train_max <- as.integer(ts$train_through_season %||% 2023)
  val_year <- as.integer(ts$validate_season %||% 2024)
  test_year <- as.integer(ts$test_season %||% 2025)
  panel$split <- dplyr::case_when(
    panel$season_year <= train_max ~ "train",
    panel$season_year == val_year ~ "validate",
    panel$season_year == test_year ~ "test",
    TRUE ~ "holdout_future"
  )
  panel
}

#' Fit ridge contribution model (glmnet). Returns NULL if packages/data insufficient.
fit_contribution_ridge <- function(panel, alpha = 0, feature_cols = NULL) {
  if (!requireNamespace("glmnet", quietly = TRUE)) {
    write_log("glmnet not installed — contribution model remains heuristic fallback.")
    return(NULL)
  }
  feature_cols <- feature_cols %||% intersect(CONTRIBUTION_FEATURE_CANDIDATES, names(panel))
  if (length(feature_cols) < 2 || nrow(panel) < 40) {
    write_log("Insufficient rows/features for contribution ridge (", nrow(panel), " rows).")
    return(NULL)
  }
  train <- panel[panel$split == "train" | is.na(panel$split), , drop = FALSE]
  if (nrow(train) < 30) train <- panel

  X <- as.matrix(train[, feature_cols, drop = FALSE])
  storage.mode(X) <- "double"
  # Simple median impute for learning only (never used as displayed neutral-50 scores)
  for (j in seq_len(ncol(X))) {
    med <- median(X[, j], na.rm = TRUE)
    if (!is.finite(med)) med <- 0
    X[is.na(X[, j]), j] <- med
  }
  y <- as.numeric(train$outcome_next)
  ok <- is.finite(y) & rowSums(!is.finite(X)) == 0
  if (sum(ok) < 30) return(NULL)

  fit <- glmnet::cv.glmnet(X[ok, , drop = FALSE], y[ok], alpha = alpha, nfolds = min(5, sum(ok)))
  list(
    model_type = if (alpha == 0) "ridge" else "elastic_net",
    alpha = alpha,
    cv_fit = fit,
    feature_cols = feature_cols,
    impute_medians = apply(X[ok, , drop = FALSE], 2, median, na.rm = TRUE),
    train_n = sum(ok),
    fitted_at = as.character(Sys.time()),
    target = "outcome_next",
    coefficient_type = "learned"
  )
}

predict_contribution_model <- function(artifact, df) {
  if (is.null(artifact) || is.null(artifact$cv_fit)) {
    return(rep(NA_real_, nrow(df)))
  }
  cols <- artifact$feature_cols
  X <- matrix(NA_real_, nrow = nrow(df), ncol = length(cols))
  colnames(X) <- cols
  for (j in seq_along(cols)) {
    col <- cols[[j]]
    if (col %in% names(df)) {
      X[, j] <- as.numeric(df[[col]])
    }
    med <- artifact$impute_medians[[col]] %||% artifact$impute_medians[[j]] %||% 0
    X[is.na(X[, j]), j] <- med
  }
  as.numeric(predict(artifact$cv_fit, newx = X, s = "lambda.min"))
}

#' Evaluate predictions on a split; returns metrics list.
evaluate_contribution_predictions <- function(actual, predicted) {
  ok <- is.finite(actual) & is.finite(predicted)
  if (sum(ok) < 5) {
    return(list(n = sum(ok), mae = NA_real_, rmse = NA_real_, rank_cor = NA_real_))
  }
  list(
    n = sum(ok),
    mae = mean(abs(actual[ok] - predicted[ok])),
    rmse = sqrt(mean((actual[ok] - predicted[ok])^2)),
    rank_cor = suppressWarnings(cor(actual[ok], predicted[ok], method = "spearman"))
  )
}

#' Score contribution: learned prediction scaled 0–100 when artifact exists; else heuristic.
score_contribution_hybrid <- function(df, role_id, spec = NULL, cfg = NULL) {
  spec <- spec %||% load_model_spec()
  art <- load_learned_artifact("contribution", cfg, spec)
  if (!is.null(art)) {
    raw <- predict_contribution_model(art, df)
    # Map raw outcome scale to 0–100 via percentile within current frame (fixed ref later)
    score <- percentile_rank(raw)
    coverage <- rep(1, nrow(df))
    return(list(
      score = score,
      coverage = coverage,
      source = "learned_ridge",
      prediction_raw = raw,
      model_type = art$model_type
    ))
  }
  # Fallback: role-specific heuristic (explicit, not pretending to be learned)
  h <- score_contribution_index_heuristic(df, role_id, spec)
  list(
    score = h$score,
    coverage = h$coverage,
    source = "heuristic_fallback",
    prediction_raw = NA_real_,
    model_type = "role_blend_heuristic"
  )
}

`%||%` <- function(a, b) if (!is.null(a)) a else b
