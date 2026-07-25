# Model 2 — League translation (LEARNED from movers; assumed tiers until then).

#' Identify players who moved into MLS from another league (simple season-join heuristic).
identify_league_movers <- function(player_seasons) {
  ensure_packages("dplyr")
  if (!all(c("asa_player_id", "season_year", "league_id") %in% names(player_seasons))) {
    return(tibble::tibble())
  }
  player_seasons |>
    dplyr::arrange(asa_player_id, season_year) |>
    dplyr::group_by(asa_player_id) |>
    dplyr::mutate(
      prev_league = dplyr::lag(league_id),
      prev_season = dplyr::lag(season_year),
      prev_gplus = dplyr::lag(goals_added_p90),
      prev_minutes = dplyr::lag(minutes),
      prev_age = dplyr::lag(age),
      prev_position = dplyr::lag(position_group)
    ) |>
    dplyr::ungroup() |>
    dplyr::filter(
      league_id == "mls",
      !is.na(prev_league),
      prev_league != "mls",
      is.finite(goals_added_p90),
      is.finite(prev_gplus)
    )
}

#' Fit a simple linear translation model on movers. Returns NULL if n too small.
fit_league_translation <- function(movers, min_n = 25) {
  if (is.null(movers) || nrow(movers) < min_n) {
    write_log("League movers n=", nrow(movers %||% tibble::tibble()),
              " < ", min_n, " — keep assumed tier priors.")
    return(NULL)
  }
  movers$source_league <- factor(movers$prev_league)
  movers$position_group <- factor(movers$prev_position %||% movers$position_group)
  fit <- tryCatch(
    lm(goals_added_p90 ~ prev_gplus + source_league + prev_age + position_group + prev_minutes,
       data = movers),
    error = function(e) NULL
  )
  if (is.null(fit)) return(NULL)
  list(
    model_type = "ols_movers",
    fit = fit,
    n = nrow(movers),
    fitted_at = as.character(Sys.time()),
    coefficient_type = "learned",
    note = "Pre→post MLS g+/90 among observed movers; CIs needed before replacing tiers."
  )
}

#' Apply translation: learned if available, else assumed tier factors already on df.
apply_league_translation_label <- function(df, cfg = NULL, spec = NULL) {
  art <- load_learned_artifact("league_translation", cfg, spec)
  if (is.null(art)) {
    df$translation_source <- "assumed_league_strength_adjustment"
  } else {
    df$translation_source <- "learned_mover_model"
  }
  df
}

`%||%` <- function(a, b) if (!is.null(a)) a else b
