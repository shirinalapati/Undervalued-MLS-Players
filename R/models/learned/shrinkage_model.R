# Model 4 — Empirical Bayes shrinkage strength (LEARNED from period-to-period reliability).

#' Estimate m0 such that shrunk rates best predict next-period rates (grid search).
estimate_shrinkage_m0 <- function(player_seasons, metric_cols = c("goals_added_p90", "npxg_p90", "xa_p90"),
                                  m0_grid = c(200, 400, 600, 900, 1200, 1800)) {
  ensure_packages("dplyr")
  if (!all(c("asa_player_id", "season_year", "minutes", "league_id", "position_group") %in% names(player_seasons))) {
    return(NULL)
  }

  results <- list()
  for (metric in intersect(metric_cols, names(player_seasons))) {
    panel <- player_seasons |>
      dplyr::filter(is.finite(.data[[metric]]), minutes > 0) |>
      dplyr::arrange(asa_player_id, season_year) |>
      dplyr::group_by(asa_player_id) |>
      dplyr::mutate(
        next_rate = dplyr::lead(.data[[metric]]),
        next_minutes = dplyr::lead(minutes)
      ) |>
      dplyr::ungroup() |>
      dplyr::filter(is.finite(next_rate), next_minutes >= 450)

    if (nrow(panel) < 40) {
      results[[metric]] <- list(m0 = 600, n = nrow(panel), status = "insufficient")
      next
    }

    priors <- panel |>
      dplyr::group_by(league_id, position_group) |>
      dplyr::summarise(prior = mean(.data[[metric]], na.rm = TRUE), .groups = "drop")

    panel <- dplyr::left_join(panel, priors, by = c("league_id", "position_group"))

    best_m0 <- 600
    best_mae <- Inf
    for (m0 in m0_grid) {
      shrunk <- empirical_bayes_shrink(panel[[metric]], panel$minutes, panel$prior, m0 = m0)
      mae <- mean(abs(shrunk - panel$next_rate), na.rm = TRUE)
      if (is.finite(mae) && mae < best_mae) {
        best_mae <- mae
        best_m0 <- m0
      }
    }
    results[[metric]] <- list(m0 = best_m0, mae = best_mae, n = nrow(panel), status = "estimated")
  }

  list(
    model_type = "eb_m0_grid_search",
    by_metric = results,
    default_m0 = {
      vals <- vapply(results, function(x) x$m0 %||% 600, numeric(1))
      as.numeric(round(mean(vals)))
    },
    fitted_at = as.character(Sys.time()),
    coefficient_type = "learned"
  )
}

#' Resolve m0 for a metric: learned artifact or config fallback.
shrinkage_m0_for <- function(metric = "goals_added_p90", cfg = NULL, spec = NULL) {
  art <- load_learned_artifact("shrinkage", cfg, spec)
  spec <- spec %||% tryCatch(load_model_spec(), error = function(e) NULL)
  if (!is.null(art$by_metric[[metric]]$m0)) return(as.numeric(art$by_metric[[metric]]$m0))
  if (!is.null(art$default_m0)) return(as.numeric(art$default_m0))
  as.numeric(spec$hybrid$learned$shrinkage$default_m0 %||% 600)
}

`%||%` <- function(a, b) if (!is.null(a)) a else b
