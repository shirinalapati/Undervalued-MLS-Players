# Model 3 — Age curves (LEARNED from historical MLS performance by position).

#' Fit quadratic age curves per position group on MLS seasons.
#' Outcome: goals_added_p90 (or other performance column).
fit_age_curves <- function(player_seasons, outcome_col = "goals_added_p90",
                           min_n_per_group = 40) {
  ensure_packages("dplyr")
  mls <- player_seasons |>
    dplyr::filter(league_id == "mls", is.finite(age), is.finite(.data[[outcome_col]]), minutes >= 450)
  if (!nrow(mls)) return(NULL)

  # Broad groups
  mls <- mls |>
    dplyr::mutate(
      age_group = dplyr::case_when(
        position_group %in% c("FW", "ST") ~ "F",
        position_group %in% c("W", "AM") ~ "W_AM",
        position_group %in% c("CM", "DM", "M") ~ "CM",
        position_group %in% c("FB", "WB") ~ "FB",
        position_group %in% c("CB", "D") ~ "CB",
        position_group == "GK" ~ "GK",
        TRUE ~ "CM"
      )
    )

  curves <- lapply(split(mls, mls$age_group), function(g) {
    if (nrow(g) < min_n_per_group) return(NULL)
    fit <- tryCatch(lm(reformulate(c("age", "I(age^2)"), outcome_col), data = g), error = function(e) NULL)
    if (is.null(fit)) return(NULL)
    ages <- seq(17, 36, by = 0.5)
    pred <- predict(fit, newdata = data.frame(age = ages))
    peak <- ages[which.max(pred)]
    list(
      position_group = g$age_group[[1]],
      peak_age = as.numeric(peak),
      coef = coef(fit),
      n = nrow(g),
      r2 = summary(fit)$r.squared
    )
  })
  curves <- Filter(Negate(is.null), curves)
  if (!length(curves)) return(NULL)

  peaks <- setNames(vapply(curves, `[[`, numeric(1), "peak_age"),
                    vapply(curves, `[[`, "", "position_group"))
  list(
    model_type = "quadratic_by_position",
    curves = curves,
    peaks = as.list(peaks),
    fitted_at = as.character(Sys.time()),
    coefficient_type = "learned",
    note = "Development input only — not current performance evidence."
  )
}

#' Peak age for development: learned peaks if present, else model_spec priors.
age_peak_hybrid <- function(position_group, spec = NULL, cfg = NULL) {
  art <- load_learned_artifact("age_curves", cfg, spec)
  spec <- spec %||% load_model_spec()
  pg <- toupper(as.character(position_group))
  map_key <- dplyr::case_when(
    pg %in% c("FW", "F", "ST") ~ "F",
    pg %in% c("W", "AM", "W_AM") ~ "W_AM",
    pg %in% c("CM", "M", "DM", "AM") ~ "CM",
    pg %in% c("FB", "WB") ~ "FB",
    pg %in% c("CB", "D") ~ "CB",
    pg == "GK" ~ "GK",
    TRUE ~ "default"
  )
  if (!is.null(art$peaks)) {
    out <- vapply(map_key, function(k) {
      as.numeric(art$peaks[[k]] %||% art$peaks$CM %||% spec$age_curves$default %||% 26.5)
    }, numeric(1))
    return(out)
  }
  age_peak_for_position(position_group, spec)
}

`%||%` <- function(a, b) if (!is.null(a)) a else b
