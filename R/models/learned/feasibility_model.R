# Model 6 — Acquisition probability (HYBRID; hard problem).
# Learned P(join MLS) when mover/non-mover labels exist; else transparent heuristic.

#' Build binary labels: joined MLS next season from non-MLS.
build_acquisition_labels <- function(player_seasons) {
  ensure_packages("dplyr")
  player_seasons |>
    dplyr::arrange(asa_player_id, season_year) |>
    dplyr::group_by(asa_player_id) |>
    dplyr::mutate(
      next_league = dplyr::lead(league_id),
      joined_mls_next = as.integer(league_id != "mls" & next_league == "mls")
    ) |>
    dplyr::ungroup() |>
    dplyr::filter(league_id != "mls", !is.na(joined_mls_next))
}

fit_acquisition_probability <- function(labeled, min_n = 100, min_positives = 15) {
  if (is.null(labeled) || nrow(labeled) < min_n) return(NULL)
  if (sum(labeled$joined_mls_next == 1) < min_positives) {
    write_log("Too few positive MLS joins for acquisition model.")
    return(NULL)
  }
  labeled$league_id <- factor(labeled$league_id)
  labeled$position_group <- factor(labeled$position_group)
  fit <- tryCatch(
    glm(joined_mls_next ~ age + minutes + league_id + position_group +
          I(pmin(minutes_share, 1)) + cost_tier,
        data = labeled, family = binomial()),
    error = function(e) NULL
  )
  if (is.null(fit)) return(NULL)
  list(
    model_type = "logistic_join_mls",
    fit = fit,
    n = nrow(labeled),
    positives = sum(labeled$joined_mls_next == 1),
    fitted_at = as.character(Sys.time()),
    coefficient_type = "learned",
    note = "Public-data proxy only; many acquisition drivers unavailable."
  )
}

#' Feasibility score: learned probability mapped 0–100, else existing heuristic.
score_feasibility_hybrid <- function(df, cfg = NULL, spec = NULL) {
  art <- load_learned_artifact("acquisition_probability", cfg, spec)
  base <- score_feasibility(df) # transparent heuristic always computed
  if (is.null(art$fit)) {
    df$score_feasibility <- base
    df$feasibility_source <- "heuristic_rules"
    return(df)
  }
  pred <- tryCatch(as.numeric(predict(art$fit, newdata = df, type = "response")), error = function(e) NULL)
  if (is.null(pred)) {
    df$score_feasibility <- base
    df$feasibility_source <- "heuristic_rules"
    return(df)
  }
  # Blend: learned probability (60%) + explicit constraints heuristic (40%)
  df$score_feasibility <- clip(0.60 * (100 * pred) + 0.40 * base, 0, 100)
  df$acquisition_probability <- pred
  df$feasibility_source <- "hybrid_learned_plus_rules"
  df
}

`%||%` <- function(a, b) if (!is.null(a)) a else b
