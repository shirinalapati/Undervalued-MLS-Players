# Incumbent vs target comparison — recommendation invariants enforced.
# Labels must agree with component deltas (see tests/test_recommendation_invariants.R).

role_metric_pct_cols <- function(role_id, roles_yaml = NULL) {
  roles_yaml <- roles_yaml %||% load_yaml("config/role_weights.yml")
  role <- roles_yaml$roles[[role_id]]
  if (is.null(role)) stop("Unknown role: ", role_id)
  weights <- unlist(role$metrics)
  cols <- paste0(names(weights), "_pct")
  list(cols = cols, weights = as.numeric(weights), metric_names = names(weights))
}

#' Pull role-adjusted metric vector. Missing stay NA (never filled with 50).
role_metric_vector <- function(player_row, role_id, roles_yaml = NULL) {
  meta <- role_metric_pct_cols(role_id, roles_yaml)
  vals <- vapply(meta$cols, function(col) {
    if (!col %in% names(player_row)) return(NA_real_)
    v <- as.numeric(player_row[[col]][[1]])
    if (!is.finite(v)) NA_real_ else v
  }, numeric(1))
  names(vals) <- meta$metric_names
  list(values = vals, weights = meta$weights / sum(meta$weights), cols = meta$cols)
}

#' Cosine similarity on jointly observed metrics only. Both missing → excluded (not equal).
cosine_sim <- function(a, b, w = NULL) {
  if (is.null(w)) w <- rep(1, length(a))
  ok <- is.finite(a) & is.finite(b) & is.finite(w)
  if (sum(ok) < 2) return(NA_real_)
  a <- a[ok] - 50; b <- b[ok] - 50; w <- w[ok]
  w <- w / sum(w)
  num <- sum(w * a * b)
  den <- sqrt(sum(w * a * a)) * sqrt(sum(w * b * b))
  if (!is.finite(den) || den < 1e-8) return(NA_real_)
  as.numeric(50 + 50 * num / den)
}

estimate_roster_pathway <- function(target_row, club = NULL) {
  league <- target_row$league_id[[1]]
  age <- target_row$age[[1]]
  cost <- target_row$cost_tier[[1]]
  intl <- if ("intl_roster_status" %in% names(target_row)) {
    target_row$intl_roster_status[[1]]
  } else {
    NA_character_
  }
  mins_share <- target_row$minutes_share[[1]] %||% 0.5

  if (identical(league, "mls")) {
    path <- if (is.finite(mins_share) && mins_share >= 0.65) {
      "MLS trade / intra-league acquisition (significant minutes already)"
    } else {
      "MLS trade or free-agent/internal pathway (lower current minutes share)"
    }
  } else if (identical(league, "mlsnp")) {
    path <- if (is.finite(age) && age <= 22) {
      "Homegrown / MLS NEXT Pro promotion pathway"
    } else {
      "MLS NEXT Pro → first-team promotion / short-term contract"
    }
  } else if (identical(league, "uslc")) {
    path <- "USL Championship transfer / discovery signing"
  } else {
    path <- "External acquisition (verify work-permit / discovery rules)"
  }

  if (identical(tolower(as.character(intl)), "international")) {
    path <- paste0(path, "; international roster slot / GAM-TAM support may be required")
  } else if (is.na(intl) || !nzchar(as.character(intl))) {
    path <- paste0(path, "; international roster status unknown (nationality is not used as a proxy)")
  }
  if (is.finite(cost) && cost >= 4) {
    path <- paste0(path, "; high guaranteed-compensation band — acquisition cost still unknown")
  } else if (!is.finite(cost)) {
    path <- paste0(path, "; compensation unknown — no fabricated cost tier")
  } else if (is.finite(cost) && cost <= 2) {
    path <- paste0(path, "; lower guaranteed-compensation band (not total acquisition cost)")
  }
  path
}

known_cost <- function(row) {
  sal <- as.numeric(row$salary[[1]])
  tier <- as.numeric(row$cost_tier[[1]])
  known_flag <- if ("compensation_known" %in% names(row)) isTRUE(as.logical(row$compensation_known[[1]])) else NA
  if (isTRUE(known_flag)) return(TRUE)
  if (isFALSE(known_flag)) return(FALSE)
  is.finite(sal) && is.finite(tier)
}

target_cheaper_than_incumbent <- function(incumbent, target) {
  if (!known_cost(incumbent) || !known_cost(target)) return(FALSE)
  sal_i <- as.numeric(incumbent$salary[[1]])
  sal_t <- as.numeric(target$salary[[1]])
  tier_i <- as.numeric(incumbent$cost_tier[[1]])
  tier_t <- as.numeric(target$cost_tier[[1]])
  # Prefer salary when both known; else cost tier
  if (is.finite(sal_i) && is.finite(sal_t)) return(sal_t < sal_i)
  is.finite(tier_i) && is.finite(tier_t) && tier_t < tier_i
}

#' Enforce hard recommendation invariants. Returns relationship label.
classify_roster_relationship <- function(scores, deltas, objective,
                                        incumbent = NULL, target = NULL,
                                        spec = NULL) {
  spec <- spec %||% tryCatch(load_model_spec(), error = function(e) NULL)
  inv <- spec$invariants %||% list()
  fallback <- spec$relationship_fallback %||% "No clear recruitment advantage identified"

  contrib_tol <- inv$lower_cost$contribution_tolerance %||% 5
  up_margin <- inv$immediate_upgrade$min_contribution_margin %||% 8
  up_role <- inv$immediate_upgrade$min_role_fit %||% 55
  up_conf <- inv$immediate_upgrade$min_confidence %||% "medium"
  dev_age <- inv$developmental_successor$min_age_gap_years %||% 1
  dev_ups <- inv$developmental_successor$min_upside %||% 60
  dev_role <- inv$developmental_successor$min_role_fit %||% 45
  dev_path <- inv$developmental_successor$min_pathway %||% 40

  d_contrib <- deltas$projected_contribution %||% NA_real_
  d_age <- deltas$age_years_younger %||% NA_real_
  tgt_role <- if (!is.null(target)) as.numeric(target$score_role_fit[[1]]) else NA_real_
  tgt_ups <- if (!is.null(target)) as.numeric(target$score_development[[1]]) else NA_real_
  tgt_conf <- if (!is.null(target)) as.character(target$confidence[[1]]) else "low"
  tgt_path <- if (!is.null(target) && "score_pathway_fit" %in% names(target)) {
    as.numeric(target$score_pathway_fit[[1]])
  } else {
    50
  }
  tgt_feas <- if (!is.null(target)) as.numeric(target$score_feasibility[[1]]) else 50

  cheaper <- FALSE
  if (!is.null(incumbent) && !is.null(target)) {
    cheaper <- target_cheaper_than_incumbent(incumbent, target)
  } else {
    cheaper <- is.finite(deltas$cost_tier_cheaper) && deltas$cost_tier_cheaper > 0 &&
      is.finite(deltas$salary_savings) # require some financial signal
  }

  finance_known <- !is.null(incumbent) && !is.null(target) &&
    known_cost(incumbent) && known_cost(target)

  passes_lower_cost <- isTRUE(finance_known) && isTRUE(cheaper) &&
    is.finite(d_contrib) && d_contrib >= -contrib_tol

  passes_upgrade <- is.finite(d_contrib) && d_contrib >= up_margin &&
    is.finite(tgt_role) && tgt_role >= up_role &&
    min_confidence_met(tgt_conf, up_conf)

  passes_successor <- is.finite(d_age) && d_age >= dev_age &&
    is.finite(tgt_ups) && tgt_ups >= dev_ups &&
    (is.na(tgt_role) || tgt_role >= dev_role) &&
    is.finite(tgt_path) && tgt_path >= dev_path &&
    is.finite(tgt_feas) && tgt_feas >= 35

  # Objective requests a category — only award if invariants pass; else fallback
  if (identical(objective, "lower_cost")) {
    if (passes_lower_cost) return("Lower-cost alternative — value-oriented swap / competition")
    return(fallback)
  }
  if (identical(objective, "upgrade")) {
    if (passes_upgrade) return("Immediate upgrade — higher estimated contribution in role")
    return(fallback)
  }
  if (identical(objective, "younger_successor")) {
    if (passes_successor) return("Developmental successor — longer-term replacement profile")
    return(fallback)
  }
  if (identical(objective, "complementary")) {
    if (is.finite(scores$complementarity) && scores$complementarity >= 55) {
      return("Complement — adds missing qualities rather than like-for-like replace")
    }
    return(fallback)
  }
  if (identical(objective, "rotation_depth")) {
    if (is.finite(scores$role_similarity) && scores$role_similarity >= 55 &&
        is.finite(d_contrib) && d_contrib < up_margin) {
      return("Depth / rotation — similar profile behind the incumbent")
    }
    return(fallback)
  }
  if (identical(objective, "direct_replacement")) {
    if (is.finite(scores$role_similarity) && scores$role_similarity >= 70) {
      return("Direct replacement — high role similarity / like-for-like")
    }
    return(fallback)
  }

  # Auto-infer: first matching invariant category
  if (passes_lower_cost) return("Lower-cost alternative — value-oriented swap / competition")
  if (passes_upgrade) return("Immediate upgrade — higher estimated contribution in role")
  if (passes_successor) return("Developmental successor — longer-term replacement profile")
  if (is.finite(scores$complementarity) && scores$complementarity >= 65 &&
      (is.na(scores$role_similarity) || scores$role_similarity < 55)) {
    return("Complement — adds missing qualities rather than like-for-like replace")
  }
  fallback
}

#' Core comparison: one incumbent vs one target at a shared tactical role.
compare_incumbent_target <- function(incumbent, target, role_id, club = NULL,
                                     objective = "upgrade", roles_yaml = NULL,
                                     roster_df = NULL) {
  ensure_packages(c("dplyr", "tibble"))
  roles_yaml <- roles_yaml %||% load_yaml("config/role_weights.yml")
  objective <- match.arg(
    objective,
    c("direct_replacement", "upgrade", "rotation_depth",
      "younger_successor", "lower_cost", "complementary")
  )

  inc_v <- role_metric_vector(incumbent, role_id, roles_yaml)
  tgt_v <- role_metric_vector(target, role_id, roles_yaml)

  # Coverage for similarity
  jointly <- is.finite(inc_v$values) & is.finite(tgt_v$values)
  sim_coverage <- sum(inc_v$weights[jointly]) / max(sum(inc_v$weights), 1e-9)
  role_similarity <- if (sim_coverage >= 0.5) {
    cosine_sim(inc_v$values, tgt_v$values, inc_v$weights)
  } else {
    NA_real_
  }

  weak <- is.finite(inc_v$values) & inc_v$values < 45 & is.finite(tgt_v$values)
  if (any(weak)) {
    excess <- pmax(tgt_v$values[weak] - inc_v$values[weak], 0)
    complementarity <- as.numeric(sum(inc_v$weights[weak] * excess) /
      max(sum(inc_v$weights[weak]), 1e-6))
    complementarity <- clip_0_100(100 * (complementarity / 40))
  } else if (any(jointly)) {
    complementarity <- clip_0_100(
      100 * mean(pmax(tgt_v$values[jointly] - inc_v$values[jointly], 0), na.rm = TRUE) / 25
    )
  } else {
    complementarity <- NA_real_
  }

  if (!is.null(roster_df) && nrow(roster_df) && any(jointly)) {
    club_means <- vapply(inc_v$cols, function(col) {
      if (!col %in% names(roster_df)) return(NA_real_)
      mean(roster_df[[col]], na.rm = TRUE)
    }, numeric(1))
    names(club_means) <- names(inc_v$values)
    missing_club <- is.finite(club_means) & club_means < 42 & is.finite(tgt_v$values)
    if (any(missing_club)) {
      fill <- mean(pmax(tgt_v$values[missing_club] - club_means[missing_club], 0), na.rm = TRUE)
      if (is.finite(complementarity)) {
        complementarity <- clip_0_100(0.65 * complementarity + 0.35 * clip_0_100(100 * fill / 30))
      }
    }
  }

  d_proj <- as.numeric(target$score_projected_mls[[1]]) - as.numeric(incumbent$score_projected_mls[[1]])
  d_role <- as.numeric(target$score_role_fit[[1]]) - as.numeric(incumbent$score_role_fit[[1]])
  d_dev <- as.numeric(target$score_development[[1]]) - as.numeric(incumbent$score_development[[1]])
  d_risk <- as.numeric(target$score_risk[[1]]) - as.numeric(incumbent$score_risk[[1]])
  d_feas <- as.numeric(target$score_feasibility[[1]]) - as.numeric(incumbent$score_feasibility[[1]])
  d_fin <- as.numeric(target$score_financial_value[[1]]) - as.numeric(incumbent$score_financial_value[[1]])
  d_age <- as.numeric(incumbent$age[[1]]) - as.numeric(target$age[[1]])
  d_cost <- as.numeric(incumbent$cost_tier[[1]]) - as.numeric(target$cost_tier[[1]])
  d_sal <- as.numeric(incumbent$salary[[1]]) - as.numeric(target$salary[[1]])

  upgrade_potential <- if (is.finite(d_proj) && is.finite(d_role)) {
    clip_0_100(50 + 0.7 * d_proj + 0.3 * d_role)
  } else if (is.finite(d_proj)) {
    clip_0_100(50 + d_proj)
  } else {
    NA_real_
  }

  financial_efficiency <- {
    if (!known_cost(incumbent) || !known_cost(target)) {
      NA_real_ # missing financial data cannot trigger a financial recommendation
    } else {
      similar_or_better <- is.finite(d_proj) && d_proj >= -5
      cheaper <- target_cheaper_than_incumbent(incumbent, target)
      base <- 50 + 0.5 * (d_fin %||% 0) + 8 * (if (is.finite(d_cost)) d_cost else 0)
      if (similar_or_better && cheaper) base <- base + 12
      if (!cheaper && is.finite(d_proj) && d_proj < 0) base <- base - 10
      as.numeric(pmin(100, pmax(0, base)))
    }
  }

  succession_value <- {
    age_bonus <- dplyr::case_when(
      !is.finite(d_age) ~ 0,
      d_age >= 5 ~ 25,
      d_age >= 3 ~ 15,
      d_age >= 1 ~ 8,
      d_age >= 0 ~ 2,
      TRUE ~ -10
    )
    as.numeric(pmin(100, pmax(0, 0.45 * as.numeric(target$score_development[[1]]) +
      0.25 * pmax(d_dev, 0) + 0.20 * (50 + age_bonus) +
      0.10 * as.numeric(target$score_feasibility[[1]]))))
  }

  # Metric deltas: both missing → NA delta (not 0)
  metric_delta <- tibble::tibble(
    metric = names(inc_v$values),
    incumbent = as.numeric(inc_v$values),
    target = as.numeric(tgt_v$values),
    delta = dplyr::if_else(
      is.finite(as.numeric(inc_v$values)) & is.finite(as.numeric(tgt_v$values)),
      as.numeric(tgt_v$values - inc_v$values),
      NA_real_
    ),
    weight = as.numeric(inc_v$weights),
    both_missing = !is.finite(as.numeric(inc_v$values)) & !is.finite(as.numeric(tgt_v$values))
  ) |>
    dplyr::arrange(dplyr::desc(abs(dplyr::coalesce(.data$delta, 0)) * .data$weight))

  deltas <- list(
    projected_contribution = d_proj,
    role_fit = d_role,
    development = d_dev,
    risk = d_risk,
    feasibility = d_feas,
    financial_value = d_fin,
    age_years_younger = d_age,
    cost_tier_cheaper = d_cost,
    salary_savings = d_sal,
    role_similarity_coverage = sim_coverage
  )

  scores <- list(
    role_similarity = if (is.finite(role_similarity)) round(role_similarity, 1) else NA_real_,
    upgrade_potential = if (is.finite(upgrade_potential)) round(upgrade_potential, 1) else NA_real_,
    complementarity = if (is.finite(complementarity)) round(complementarity, 1) else NA_real_,
    financial_efficiency = if (is.finite(financial_efficiency)) round(financial_efficiency, 1) else NA_real_,
    succession_value = round(succession_value, 1)
  )

  relationship <- classify_roster_relationship(
    scores, deltas, objective, incumbent = incumbent, target = target
  )
  pathway <- estimate_roster_pathway(target, club)
  narrative <- write_roster_impact_narrative(
    incumbent, target, role_id, objective, scores, deltas, relationship, pathway, metric_delta
  )

  inc_name <- as.character(incumbent$display_name[[1]])
  tgt_name <- as.character(target$display_name[[1]])
  inc_age <- round(as.numeric(incumbent$age[[1]]), 1)
  tgt_age <- round(as.numeric(target$age[[1]]), 1)
  inc_minutes <- round(as.numeric(incumbent$minutes[[1]]))
  tgt_minutes <- round(as.numeric(target$minutes[[1]]))
  inc_tier <- as.integer(incumbent$cost_tier[[1]])
  tgt_tier <- as.integer(target$cost_tier[[1]])
  inc_salary <- as.numeric(incumbent$salary[[1]])
  tgt_salary <- as.numeric(target$salary[[1]])
  inc_proj <- round(as.numeric(incumbent$score_projected_mls[[1]]), 1)
  tgt_proj <- round(as.numeric(target$score_projected_mls[[1]]), 1)
  tgt_feas <- round(as.numeric(target$score_feasibility[[1]]), 1)
  tgt_risk <- round(as.numeric(target$score_risk[[1]]), 1)
  tgt_conf <- as.character(target$confidence[[1]])

  list(
    incumbent_name = inc_name,
    target_name = tgt_name,
    incumbent_id = incumbent$asa_player_id[[1]],
    target_id = target$asa_player_id[[1]],
    role_id = role_id,
    objective = objective,
    relationship = relationship,
    scores = scores,
    deltas = deltas,
    metric_delta = metric_delta,
    pathway = pathway,
    narrative = narrative,
    incumbent = incumbent,
    target = target,
    summary_row = tibble::tibble(
      incumbent = inc_name,
      target = tgt_name,
      role_id = role_id,
      objective = objective,
      relationship = relationship,
      role_similarity = scores$role_similarity,
      upgrade_potential = scores$upgrade_potential,
      complementarity = scores$complementarity,
      financial_efficiency = scores$financial_efficiency,
      succession_value = scores$succession_value,
      delta_projected = round(d_proj, 1),
      delta_role_fit = round(d_role, 1),
      delta_development = round(d_dev, 1),
      delta_risk = round(d_risk, 1),
      age_years_younger = round(d_age, 1),
      cost_tier_cheaper = d_cost,
      incumbent_age = inc_age,
      target_age = tgt_age,
      incumbent_minutes = inc_minutes,
      target_minutes = tgt_minutes,
      incumbent_cost_tier = inc_tier,
      target_cost_tier = tgt_tier,
      incumbent_salary = inc_salary,
      target_salary = tgt_salary,
      incumbent_proj = inc_proj,
      target_proj = tgt_proj,
      target_feasibility = tgt_feas,
      target_risk = tgt_risk,
      target_confidence = tgt_conf,
      pathway = pathway,
      narrative = narrative
    )
  )
}

fmt_delta <- function(x, suffix = "", digits = 0) {
  if (!is.finite(x)) return("Not available")
  sign <- if (x > 0) "+" else ""
  paste0(sign, formatC(x, format = "f", digits = digits), suffix)
}

write_roster_impact_narrative <- function(incumbent, target, role_id, objective,
                                          scores, deltas, relationship, pathway,
                                          metric_delta) {
  role_label <- gsub("_", " ", role_id)
  top_gains <- metric_delta |>
    dplyr::filter(is.finite(.data$delta), .data$delta > 3) |>
    dplyr::slice_head(n = 3)
  top_losses <- metric_delta |>
    dplyr::filter(is.finite(.data$delta), .data$delta < -3) |>
    dplyr::slice_head(n = 3)
  n_both_missing <- sum(metric_delta$both_missing %||% FALSE, na.rm = TRUE)

  improves <- if (nrow(top_gains)) {
    paste(sprintf(
      "%s (%s)",
      top_gains$metric,
      vapply(top_gains$delta, function(d) fmt_delta(d, digits = 0), character(1))
    ), collapse = "; ")
  } else {
    "limited observed metric-level gains versus the incumbent on role-weighted traits"
  }

  loses <- if (nrow(top_losses)) {
    paste(sprintf(
      "%s (%s)",
      top_losses$metric,
      vapply(top_losses$delta, function(d) fmt_delta(d, digits = 0), character(1))
    ), collapse = "; ")
  } else {
    "little clear loss on observed role-weighted traits versus the incumbent"
  }

  fin_line <- if (!known_cost(incumbent) || !known_cost(target)) {
    "Compensation unknown for one or both players — no financial recommendation."
  } else if (is.finite(deltas$cost_tier_cheaper) && deltas$cost_tier_cheaper > 0) {
    sprintf("Target sits %s guaranteed-compensation tier(s) lower — compensation-adjusted efficiency %.0f (not acquisition surplus).",
            deltas$cost_tier_cheaper, scores$financial_efficiency %||% NA_real_)
  } else if (is.finite(deltas$cost_tier_cheaper) && deltas$cost_tier_cheaper < 0) {
    sprintf("Target is %s guaranteed-compensation tier(s) higher — cannot be labeled lower-cost (efficiency %s).",
            abs(deltas$cost_tier_cheaper),
            if (is.finite(scores$financial_efficiency)) sprintf("%.0f", scores$financial_efficiency) else "n/a")
  } else {
    sprintf("Similar guaranteed-compensation tier; efficiency %s.",
            if (is.finite(scores$financial_efficiency)) sprintf("%.0f", scores$financial_efficiency) else "n/a")
  }

  uncertainty <- paste0(
    "Main uncertainty: ",
    if (as.numeric(target$score_risk[[1]]) >= 60) {
      "elevated model uncertainty on the target"
    } else if (identical(target$league_id[[1]], "mls") &&
               (target$minutes_share[[1]] %||% 0) > 0.7) {
      "acquiring a high-minute MLS incumbent may be difficult / expensive"
    } else if ((target$minutes[[1]] %||% 0) < 700) {
      "limited minutes — YTD/sample reliability is thin"
    } else {
      "role translation and tactical fit must be confirmed on video"
    },
    sprintf(" (model uncertainty %.0f, confidence %s).", target$score_risk[[1]], target$confidence[[1]])
  )

  video <- target$video_questions[[1]] %||%
    "Verify pressing intensity, first touch under pressure, and decision speed in transition."

  paste0(
    "ROSTER IMPACT — ", target$display_name[[1]], " vs ", incumbent$display_name[[1]],
    " (", role_label, ")\n",
    "Objective: ", objective, " | Relationship: ", relationship, "\n\n",
    "What the target improves (observed metrics only): ", improves, ". ",
    "Estimated contribution delta ", fmt_delta(deltas$projected_contribution, digits = 0),
    "; role fit ", fmt_delta(deltas$role_fit, digits = 0),
    "; development ", fmt_delta(deltas$development, digits = 0),
    "; age ", fmt_delta(deltas$age_years_younger, " years younger", 1), ".\n",
    if (n_both_missing > 0) {
      sprintf("Note: %d role metric(s) unavailable for both players — not treated as equal.\n\n", n_both_missing)
    } else {
      "\n"
    },
    "What the club would lose: ", loses, ".\n\n",
    "Replacement vs complement: ", relationship,
    sprintf(" (role similarity %s; complementarity %s; upgrade potential %s; succession %.0f).\n\n",
            fmt_na_display(scores$role_similarity, 0),
            fmt_na_display(scores$complementarity, 0),
            fmt_na_display(scores$upgrade_potential, 0),
            scores$succession_value),
    "Financial implications: ", fin_line, "\n",
    "Estimated pathway: ", pathway, "\n\n",
    uncertainty, "\n\n",
    "Video verification: ", video
  )
}

compare_target_to_incumbents <- function(incumbents_df, target_row, role_id, club = NULL,
                                         objective = "upgrade", roles_yaml = NULL,
                                         roster_df = NULL) {
  ensure_packages("purrr")
  if (!nrow(incumbents_df)) stop("Select at least one current roster player.")
  comps <- lapply(seq_len(nrow(incumbents_df)), function(i) {
    compare_incumbent_target(
      incumbents_df[i, , drop = FALSE],
      target_row,
      role_id = role_id,
      club = club,
      objective = objective,
      roles_yaml = roles_yaml,
      roster_df = roster_df
    )
  })
  summary <- dplyr::bind_rows(lapply(comps, `[[`, "summary_row"))
  list(comparisons = comps, summary = summary)
}

clip_0_100 <- function(x) {
  as.numeric(pmin(100, pmax(0, x)))
}

export_roster_comparisons_xlsx <- function(comparison_bundle, path, provenance = NULL) {
  ensure_packages("openxlsx")
  spec <- tryCatch(load_model_spec(), error = function(e) NULL)
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "Comparisons")
  openxlsx::writeData(wb, "Comparisons", comparison_bundle$summary)

  n <- min(length(comparison_bundle$comparisons), 6)
  for (i in seq_len(n)) {
    cmp <- comparison_bundle$comparisons[[i]]
    sheet <- substr(paste0("Metrics_", i, "_", gsub("[^A-Za-z0-9]", "", cmp$target_name)), 1, 28)
    openxlsx::addWorksheet(wb, sheet)
    openxlsx::writeData(wb, sheet, cmp$metric_delta)
    openxlsx::writeData(wb, sheet, data.frame(narrative = cmp$narrative), startRow = nrow(cmp$metric_delta) + 3)
  }

  openxlsx::addWorksheet(wb, "Disclaimer")
  openxlsx::writeData(
    wb, "Disclaimer",
    data.frame(
      note = c(
        paste0("Model version: ", spec$model_version %||% "unknown",
               " (", spec$status %||% "unvalidated_heuristic", ")"),
        "Public-data roster comparison — not an official club document.",
        paste(source_cutoff_labels(provenance %||% list()), collapse = " | "),
        "Estimated Near-Term Contribution Index is unvalidated — not a precise projection.",
        "Compensation-Adjusted Value Index uses guaranteed compensation when known — not acquisition surplus.",
        "Role-adjusted metrics only; proxy metrics are labeled; missing = Not available."
      )
    )
  )
  openxlsx::saveWorkbook(wb, path, overwrite = TRUE)
  invisible(path)
}

`%||%` <- function(a, b) if (!is.null(a)) a else b
