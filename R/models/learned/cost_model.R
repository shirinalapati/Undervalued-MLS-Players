# Model 5 — Estimated cost / compensation tier (LEARNED when labels exist).
# Observed MLSPA guaranteed compensation always wins over estimates.

#' Fit ordinal/linear cost-tier model on players with known compensation.
fit_cost_tier_model <- function(players_df, min_n = 80) {
  ensure_packages("dplyr")
  labeled <- players_df |>
    dplyr::filter(is.finite(salary), is.finite(cost_tier), isTRUE(compensation_known) | is.finite(salary))
  if (!"compensation_known" %in% names(players_df)) {
    labeled <- players_df |> dplyr::filter(is.finite(salary), is.finite(cost_tier))
  }
  if (nrow(labeled) < min_n) {
    write_log("Cost-tier labeled n=", nrow(labeled), " — leave unknown when unobserved.")
    return(NULL)
  }
  labeled$league_id <- factor(labeled$league_id)
  labeled$position_group <- factor(labeled$position_group)
  fit <- tryCatch(
    lm(cost_tier ~ age + minutes + league_id + position_group + goals_added_p90,
       data = labeled),
    error = function(e) NULL
  )
  if (is.null(fit)) return(NULL)
  list(
    model_type = "ols_cost_tier",
    fit = fit,
    n = nrow(labeled),
    fitted_at = as.character(Sys.time()),
    coefficient_type = "learned",
    label = "Estimated cost tier (not transfer fee)"
  )
}

#' Attach estimated_acquisition_cost_tier only when compensation unknown.
apply_cost_estimates <- function(df, cfg = NULL, spec = NULL) {
  art <- load_learned_artifact("cost_tier", cfg, spec)
  if (is.null(art$fit)) {
    if (!"estimated_acquisition_cost_tier" %in% names(df)) {
      df$estimated_acquisition_cost_tier <- NA_integer_
    }
    df$cost_estimate_source <- "none"
    return(df)
  }
  need <- !is.finite(df$salary) | (isFALSE(df$compensation_known %||% TRUE) & !is.finite(df$salary))
  # Safer: estimate only where salary missing
  need <- !is.finite(df$salary)
  pred <- rep(NA_real_, nrow(df))
  if (any(need)) {
    nd <- df[need, , drop = FALSE]
    # Align factor levels roughly
    pred[need] <- tryCatch(as.numeric(predict(art$fit, newdata = nd)), error = function(e) NA_real_)
  }
  # Never overwrite known guaranteed compensation with an estimate
  df$estimated_acquisition_cost_tier <- as.integer(round(clip(pred, 1, 5)))
  df$cost_estimate_source <- ifelse(need & is.finite(pred), "learned_estimate", "observed_or_unknown")
  df
}

`%||%` <- function(a, b) if (!is.null(a)) a else b
