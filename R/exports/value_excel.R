# Excel export for 2026 MLS Value Index

export_value_index_xlsx <- function(scores, path, vi_cfg = NULL, provenance = NULL) {
  ensure_packages(c("openxlsx", "dplyr"))
  vi_cfg <- vi_cfg %||% load_value_index_config()
  counts <- value_index_counts(scores)
  official <- scores |>
    dplyr::filter(official_eligible) |>
    dplyr::arrange(display_rank, dplyr::desc(undervaluation_score), dplyr::desc(value_surplus))

  wb <- openxlsx::createWorkbook()

  add_sheet <- function(name, df) {
    openxlsx::addWorksheet(wb, name)
    openxlsx::writeDataTable(wb, name, df, tableStyle = "TableStyleMedium2")
    openxlsx::freezePane(wb, name, firstRow = TRUE)
    openxlsx::setColWidths(wb, name, cols = seq_len(ncol(df)), widths = "auto")
  }

  exec <- data.frame(
    Item = c(
      "Project", "Question", "Players evaluated",
      "Official eligible", "Data cutoff", "Generated"
    ),
    Value = c(
      vi_cfg$model$name %||% "2026 MLS Value Index",
      value_index_research_question(vi_cfg),
      counts$n_players_evaluated,
      counts$n_official_eligible,
      provenance$data_cutoff_utc %||% scores$data_cutoff_label[[1]] %||% "",
      format(Sys.time(), tz = "UTC", usetz = TRUE)
    ),
    stringsAsFactors = FALSE
  )
  add_sheet("Executive Summary", exec)

  rank_export <- official |>
    dplyr::transmute(
      `Display Rank` = display_rank,
      `Position Rank` = dplyr::coalesce(position_rank, undervaluation_rank),
      Player = display_name,
      Club = club,
      Position = position_label,
      Age = round(age, 1),
      `2026 Minutes` = round(minutes_2026),
      `Guaranteed Compensation` = compensation,
      `Sporting Impact` = round(sporting_impact, 1),
      `Compensation Percentile` = round(compensation_percentile, 1),
      `Value Surplus` = round(value_surplus, 1),
      `Undervaluation Score` = round(undervaluation_score, 1),
      `Data Confidence` = data_confidence,
      `Value Label` = value_label
    )
  add_sheet("Overall Rankings", rank_export)

  for (pg in sort(unique(official$position_group))) {
    sub <- rank_export[official$position_group == pg, , drop = FALSE]
    sheet <- substr(gsub("[^A-Za-z0-9 ]", "", position_group_label(pg, vi_cfg)), 1, 28)
    if (!nzchar(sheet)) sheet <- pg
    base <- sheet
    i <- 1
    while (sheet %in% names(wb)) {
      sheet <- paste0(base, i)
      i <- i + 1
    }
    add_sheet(sheet, sub)
  }

  team <- official |>
    dplyr::group_by(club) |>
    dplyr::summarise(
      n_players = dplyr::n(),
      median_sporting_impact = median(sporting_impact, na.rm = TRUE),
      median_compensation_percentile = median(compensation_percentile, na.rm = TRUE),
      median_value_surplus = median(value_surplus, na.rm = TRUE),
      total_known_compensation = sum(compensation, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::arrange(dplyr::desc(median_value_surplus))
  add_sheet("Team Value", team)

  p96 <- vi_cfg$shrinkage$p96_factor %||% (96 / 90)
  profile <- official |>
    dplyr::transmute(
      Player = display_name,
      Club = club,
      Position = position_label,
      Age = round(age, 1),
      `2026 Minutes` = round(minutes_2026),
      `2025 Minutes` = round(minutes_2025),
      `Guaranteed Compensation` = compensation,
      `Sporting Impact` = round(sporting_impact, 1),
      `Compensation Percentile` = round(compensation_percentile, 1),
      `Value Surplus` = round(value_surplus, 1),
      `Undervaluation Score` = round(undervaluation_score, 1),
      `Data Confidence` = data_confidence,
      `Model Confidence` = round(dplyr::coalesce(model_confidence, 100 - model_uncertainty), 1),
      `Value Label` = value_label,
      `Adjusted Goals Added per 96` = round(adjusted_goals_added_p96, 3),
      `Shooting G+/96` = round(dplyr::coalesce(goals_added_shooting_p96, goals_added_shooting_p90 * p96), 3),
      `Passing G+/96` = round(dplyr::coalesce(goals_added_passing_p96, goals_added_passing_p90 * p96), 3),
      `Receiving G+/96` = round(dplyr::coalesce(goals_added_receiving_p96, goals_added_receiving_p90 * p96), 3),
      `Dribbling G+/96` = round(dplyr::coalesce(goals_added_dribbling_p96, goals_added_dribbling_p90 * p96), 3),
      `Interrupting G+/96` = round(dplyr::coalesce(goals_added_defending_p96, goals_added_defending_p90 * p96), 3),
      `Fouling G+/96` = round(dplyr::coalesce(goals_added_fouling_p96, goals_added_fouling_p90 * p96), 3)
    )
  add_sheet("Player Profiles", profile)

  dict <- data.frame(
    Display = c(
      "Position-Adjusted Sporting Impact", "Compensation Percentile",
      "Value Surplus", "Undervaluation Score", "Display Rank", "Position Rank",
      "Data Confidence", "Model Confidence"
    ),
    Definition = c(
      "Position-group percentile of reliability-adjusted blended total G+/96",
      "Position-group percentile of 2026 guaranteed compensation",
      "Sporting Impact − Compensation Percentile",
      "Position-group percentile of Value Surplus among official eligible players",
      "Sequential rank across all official eligible players (does not reset by position)",
      "Rank within position group among official eligible players",
      "Minutes, coverage, and prior availability summary",
      "100 − Model Uncertainty; higher means more stable evidence"
    ),
    stringsAsFactors = FALSE
  )
  add_sheet("Metric Dictionary", dict)

  method <- data.frame(
    Topic = c(
      "Research question", "Primary measure", "Blend", "Shrinkage", "Eligibility",
      "Value labels", "Not estimated"
    ),
    Detail = c(
      value_index_research_question(vi_cfg),
      "Total Goals Added per 96 (position-adjusted on-ball impact; components explanatory only)",
      "w2026 = minutes_2026 / (minutes_2026 + 700), clamped 0.15–0.90",
      "EB shrink with prior_strength = 600 minutes toward position prior",
      "MLS, known 2026 pay, Sporting Impact available, >=450 2026 minutes",
      paste(
        "Elite: surplus>=25, Undervaluation Score>=90, Impact>=70, Med/High confidence;",
        "Strong: surplus>=15, Undervaluation Score>=75, Impact>=60, Med/High;",
        "Undervalued: surplus>=5, Undervaluation Score>=60, Impact>=55;",
        "Fair: surplus in (-15,5) or fails higher-label floors;",
        "Below Expected: surplus<=-15.",
        "No Undervalued/Strong/Elite when surplus<=0."
      ),
      "Adjusted team impact, off-ball modeling, plus-minus, transfer fees, GAM, SBC, trade value"
    ),
    stringsAsFactors = FALSE
  )
  add_sheet("Methodology", method)

  sources <- data.frame(
    Source = c("American Soccer Analysis", "MLSPA via ASA"),
    Use = c("Goals Added and performance rates", "2026 guaranteed compensation"),
    stringsAsFactors = FALSE
  )
  add_sheet("Data Sources", sources)

  lim <- data.frame(
    Limitation = c(
      "Public Goals Added measures measurable on-ball impact, not every form of player contribution",
      "Guaranteed compensation is not Salary Budget Charge or acquisition cost",
      "Partial-season 2026 minutes increase uncertainty",
      "Goalkeepers excluded pending comparable public metrics",
      "Descriptive ranking — not a guaranteed forecast of breakouts"
    ),
    stringsAsFactors = FALSE
  )
  add_sheet("Limitations", lim)

  openxlsx::saveWorkbook(wb, path, overwrite = TRUE)
  invisible(path)
}
