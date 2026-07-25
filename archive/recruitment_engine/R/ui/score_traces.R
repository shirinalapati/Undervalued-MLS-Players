# Player-level "How was this calculated?" traces for the Profile tab.

feasibility_trace_table <- function(player_row) {
  p <- as.list(player_row[1, , drop = FALSE])
  cost_tier <- suppressWarnings(as.numeric(p$cost_tier))
  cost_score <- dplyr::case_when(
    !is.finite(cost_tier) ~ 45,
    cost_tier == 1 ~ 95,
    cost_tier == 2 ~ 80,
    cost_tier == 3 ~ 55,
    cost_tier == 4 ~ 30,
    TRUE ~ 10
  )
  cost_note <- if (!is.finite(cost_tier)) "Unknown compensation → stand-in 45 (confidence reduced)" else paste0("Tier ", cost_tier)

  lg <- tolower(as.character(p$league_id %||% ""))
  league_score <- dplyr::case_when(
    lg == "uslc" ~ 85,
    lg == "mlsnp" ~ 80,
    lg == "mls" ~ 60,
    TRUE ~ 50
  )

  share <- suppressWarnings(as.numeric(p$minutes_share %||% NA_real_))
  mins <- suppressWarnings(as.numeric(p$minutes %||% NA_real_))
  if ((!is.finite(share) || length(share) == 0) && is.finite(mins)) {
    share <- mins / 3060
  }
  if (!is.finite(share)) share <- NA_real_
  minutes_score <- if (identical(lg, "mls") && is.finite(share) && share > 0.75) 45 else 75
  minutes_note <- if (identical(lg, "mls") && is.finite(share) && share > 0.75) {
    "MLS regular (high minutes share)"
  } else {
    "Not treated as entrenched MLS regular"
  }

  intl_raw <- p$intl_roster_status %||% NA
  intl <- tolower(as.character(intl_raw))
  if (length(intl) == 0 || is.na(intl_raw) || !nzchar(intl) || identical(intl, "na")) {
    domestic_score <- 60
    domestic_note <- "Official roster status Unknown → stand-in 60"
  } else if (intl %in% c("domestic", "homegrown")) {
    domestic_score <- 80
    domestic_note <- paste0("Roster status: ", intl)
  } else if (identical(intl, "international")) {
    domestic_score <- 55
    domestic_note <- paste0("Roster status: ", intl)
  } else {
    domestic_score <- 60
    domestic_note <- paste0("Roster status: ", intl)
  }

  data.frame(
    Input = c("Compensation / cost tier", "Current league", "Minutes situation", "Roster / domestic signal"),
    Value = c(cost_note, toupper(lg), minutes_note, domestic_note),
    Score = c(cost_score, league_score, minutes_score, domestic_score),
    Weight = c("35%", "30%", "20%", "15%"),
    stringsAsFactors = FALSE
  )
}

uncertainty_trace_parts <- function(player_row) {
  p <- as.list(player_row[1, , drop = FALSE])
  mins <- suppressWarnings(as.numeric(p$minutes))
  sample_u <- dplyr::case_when(
    !is.finite(mins) | mins < 700 ~ 80,
    mins < 1200 ~ 55,
    mins < 2000 ~ 35,
    TRUE ~ 20
  )
  tf <- suppressWarnings(as.numeric(p$tf_uncertainty))
  if (!is.finite(tf)) tf <- 0.15
  translation_u <- 100 * tf
  cov <- suppressWarnings(as.numeric(p$role_metric_coverage))
  if (!is.finite(cov)) cov <- 1
  missing_u <- max(0, min(100, 100 * (1 - cov)))
  total <- 0.40 * sample_u + 0.35 * translation_u + 0.25 * missing_u
  list(sample_u = sample_u, translation_u = translation_u, missing_u = missing_u, total = total, coverage = cov, minutes = mins)
}

value_trace <- function(player_row) {
  p <- as.list(player_row[1, , drop = FALSE])
  contrib <- suppressWarnings(as.numeric(p$score_projected_mls %||% p$score_contribution_index))
  tier <- suppressWarnings(as.numeric(p$cost_tier))
  penalty <- dplyr::case_when(
    !is.finite(tier) ~ NA_real_,
    tier == 1 ~ 20,
    tier == 2 ~ 40,
    tier == 3 ~ 60,
    tier == 4 ~ 80,
    TRUE ~ 95
  )
  value <- if (is.finite(contrib) && is.finite(penalty)) max(0, min(100, contrib - penalty + 50)) else NA_real_
  list(contrib = contrib, tier = tier, penalty = penalty, value = value)
}

render_player_score_traces <- function(player_row, role_id, roles_yaml = NULL, spec = NULL) {
  if (is.null(player_row) || !nrow(as.data.frame(player_row))) {
    return(tags$p("Select a player to see calculation traces."))
  }
  name <- player_row$display_name[[1]]
  role_id <- role_id %||% player_row$role_id[[1]]

  rf <- role_fit_player_trace(player_row, role_id, roles_yaml = roles_yaml, spec = spec)
  vt <- value_trace(player_row)
  ft <- feasibility_trace_table(player_row)
  ut <- uncertainty_trace_parts(player_row)
  feas_final <- suppressWarnings(as.numeric(player_row$score_feasibility[[1]]))

  tagList(
    h4(sprintf("How was this calculated? — %s", name)),
    about_accordion(
      "Role Fit trace",
      render_role_fit_trace_ui(rf),
      tags$p(class = "help-text",
             "Observed metric → (optional shrinkage/league adj. in features) → percentile → role weight → Role Fit.")
    ),
    about_accordion(
      "Compensation-Adjusted Value trace",
      if (!is.finite(vt$penalty)) {
        tags$p("Compensation unknown → Value = Not available. Guaranteed compensation only — not transfer or trade cost.")
      } else {
        tags$ul(
          tags$li(sprintf("Estimated Contribution: %.1f", vt$contrib)),
          tags$li(sprintf("Known compensation tier: %s", vt$tier)),
          tags$li(sprintf("Configured cost penalty: %.0f", vt$penalty)),
          tags$li(sprintf("Value = %.1f − %.0f + 50 → Final Value: %.1f", vt$contrib, vt$penalty, vt$value))
        )
      }
    ),
    about_accordion(
      "Acquisition Feasibility trace",
      div(style = "overflow-x:auto;",
        tags$table(
          class = "about-table",
          tags$thead(tags$tr(lapply(c("Input", "Value", "Score", "Weight"), tags$th))),
          tags$tbody(lapply(seq_len(nrow(ft)), function(i) {
            tags$tr(lapply(ft[i, ], function(x) tags$td(as.character(x))))
          }))
        )
      ),
      p(HTML(sprintf(
        "Weighted Feasibility (heuristic): <strong>%s</strong>. Gate and confidence still apply when roster/contract fields are Unknown.",
        if (is.finite(feas_final)) sprintf("%.1f", feas_final) else "—"
      ))),
      tags$p(class = "help-text",
             "When contract or official roster status is unavailable, stand-in scores are shown explicitly — they are not hidden.")
    ),
    about_accordion(
      "Model Uncertainty trace",
      tags$ul(
        tags$li(sprintf("Sample uncertainty: %.0f (minutes = %s)", ut$sample_u,
                        if (is.finite(ut$minutes)) round(ut$minutes) else "unknown")),
        tags$li(sprintf("League-translation uncertainty: %.1f", ut$translation_u)),
        tags$li(sprintf("Missing-metric uncertainty: %.1f (coverage = %.0f%%)", ut$missing_u, 100 * ut$coverage))
      ),
      p(HTML(sprintf(
        "Model Uncertainty = 40%% × sample + 35%% × translation + 25%% × missingness = <strong>%.1f</strong> (lower is better).",
        ut$total
      )))
    ),
    about_accordion(
      "Contribution pipeline (conceptual order)",
      tags$ol(
        tags$li("Observed performance rates"),
        tags$li("Reliability / small-sample adjustment (shrinkage)"),
        tags$li("League-strength adjustment (assumed factors)"),
        tags$li("MLS reference percentile"),
        tags$li("Role-specific contribution blend"),
        tags$li("Minutes factor → final Estimated Contribution")
      ),
      p(HTML(sprintf(
        "Stored Estimated Contribution for this player: <strong>%s</strong>.",
        {
          v <- suppressWarnings(as.numeric(player_row$score_projected_mls[[1]]))
          if (is.finite(v)) sprintf("%.1f", v) else "Not available"
        }
      )))
    )
  )
}
