#!/usr/bin/env Rscript
# 06_generate_rankings.R

source("R/utilities/load_project.R")
source("R/utilities/data_provenance.R")
source("R/models/component_scores.R")
source("R/models/club_conditioning.R")
source("R/rankings/rank_helpers.R")

cfg <- load_config()
ensure_packages(c("readr", "dplyr", "purrr", "jsonlite", "yaml", "openxlsx"))
prov <- read_provenance(cfg)

scored <- readr::read_csv(file.path(cfg$paths$processed, "player_component_scores.csv"), show_col_types = FALSE)
scored$data_cutoff_label <- cutoff_label(prov)
scored$is_synthetic <- isTRUE(prov$is_synthetic)
clubs_yaml <- load_yaml("config/club_profiles.yml")
clubs <- clubs_yaml$clubs
run_id <- paste0("run_", format(Sys.time(), "%Y%m%d_%H%M%S"))

role_neutral <- purrr::map_dfr(unique(scored$role_id), function(role) {
  d <- filter_role_pool(scored, role)
  d <- apply_overall_score(d, cfg$scoring$weights, cfg,
                           apply_risk = isTRUE(cfg$scoring$apply_risk_penalty),
                           risk_lambda = cfg$scoring$risk_penalty_weight)
  rank_shortlist(d, n = 50) |>
    dplyr::mutate(
      run_id = run_id,
      club_id = NA_character_,
      ranking_type = "role_neutral",
      score_club_fit = NA_real_,
      weights_used = NA_character_,
      explanation = strengths,
      case_priority = NA_character_
    )
})

case_clubs <- list(
  list(id = "rbny", priority = "development"),
  list(id = "columbus", priority = "immediate"),
  list(id = "colorado", priority = "balanced")
)

find_club <- function(id) {
  for (c in clubs) {
    if (identical(c$club_id, id) || identical(tolower(c$club_id), tolower(id))) return(c)
  }
  NULL
}

case_rankings <- purrr::map_dfr(case_clubs, function(cc) {
  club <- find_club(cc$id)
  if (is.null(club) && cc$id == "rbny") club <- find_club("rbnY")
  if (is.null(club)) return(NULL)

  role <- dplyr::case_when(
    grepl("press", club$tactical_archetype) ~ "pressing_striker",
    grepl("possession", club$tactical_archetype) ~ "ball_winning_midfielder",
    TRUE ~ "transition_winger"
  )

  d <- filter_role_pool(scored, role)
  if (!is.null(club$financial_value_weight) && club$financial_value_weight >= 0.65) {
    d <- dplyr::filter(d, cost_tier <= 2)
  }
  if (!is.null(club$development_priority) && club$development_priority >= 0.65) {
    d <- dplyr::filter(d, age <= 25)
  }
  if (!nrow(d)) return(NULL)

  d <- club_conditioned_score(d, club, cfg$scoring$weights, priority = cc$priority)
  if (isTRUE(cfg$scoring$apply_risk_penalty)) {
    d$score_overall <- d$score_overall * (1 - cfg$scoring$risk_penalty_weight * d$score_risk / 100)
  }
  rank_shortlist(d, n = cfg$project$shortlist_size %||% 15) |>
    dplyr::mutate(
      run_id = run_id,
      club_id = club$club_id,
      ranking_type = "case_study",
      case_priority = cc$priority,
      feasibility_tier = feasibility_tier_label(score_feasibility)
    )
})

dir_create_safe(cfg$paths$processed)
dir_create_safe(cfg$paths$exports)

readr::write_csv(role_neutral, file.path(cfg$paths$processed, "rankings_role_neutral.csv"))
readr::write_csv(case_rankings, file.path(cfg$paths$processed, "rankings_case_studies.csv"))

if (nrow(case_rankings)) {
  export_rankings <- case_rankings
  fname_prefix <- "shortlists_"
  if (isTRUE(prov$is_synthetic)) {
    export_rankings <- export_rankings |>
      dplyr::mutate(
        display_name = paste0("[DEMO] ", display_name),
        export_disclaimer = "SYNTHETIC DEMO DATA — NOT A GENUINE SCOUTING REPORT",
        data_cutoff_label = cutoff_label(prov)
      )
    fname_prefix <- "DEMO_ONLY_shortlists_"
    write_log("Synthetic mode: watermarking Excel export and prefixing DEMO_ONLY_.")
  } else {
    export_rankings <- export_rankings |>
      dplyr::mutate(
        data_cutoff_label = cutoff_label(prov),
        export_disclaimer = "Live shortlist. 2026 stats are season-to-date unless period is 2025 full season."
      )
  }

  sheets <- split(export_rankings, export_rankings$club_id)
  wb <- openxlsx::createWorkbook()
  for (nm in names(sheets)) {
    openxlsx::addWorksheet(wb, substr(nm, 1, 31))
    export_cols <- c(
      "rank", "display_name", "league_id", "age", "position_group", "role_id",
      "score_overall", "score_club_fit", "score_projected_mls", "score_role_fit",
      "score_feasibility", "feasibility_tier", "score_development", "score_financial_value",
      "score_risk", "recommendation", "strengths", "risks_text", "explanation",
      "data_cutoff_label", "export_disclaimer"
    )
    export_cols <- intersect(export_cols, names(sheets[[nm]]))
    openxlsx::writeData(wb, substr(nm, 1, 31), sheets[[nm]][, export_cols])
  }
  openxlsx::saveWorkbook(
    wb,
    file.path(cfg$paths$exports, paste0(fname_prefix, run_id, ".xlsx")),
    overwrite = TRUE
  )
}

if (file.exists(cfg$paths$database)) {
  source("R/database/db_utils.R")
  con <- db_connect(cfg)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  to_db <- dplyr::bind_rows(
    role_neutral |> dplyr::mutate(
      weights_used = as.character(weights_used),
      explanation = as.character(dplyr::coalesce(explanation, strengths))
    ),
    case_rankings |> dplyr::mutate(
      weights_used = as.character(weights_used),
      explanation = as.character(explanation)
    )
  ) |>
    dplyr::transmute(
      run_id, club_id, role_id, player_id, season_year, rank, score_overall,
      score_club_fit = dplyr::coalesce(as.numeric(score_club_fit), NA_real_),
      score_projected_mls, score_role_fit, score_feasibility, score_development,
      score_financial_value, score_risk, recommendation, explanation,
      weights_json = dplyr::coalesce(weights_used, NA_character_)
    )
  DBI::dbWriteTable(con, "scouting_rankings", as.data.frame(to_db), append = TRUE)
  DBI::dbWriteTable(
    con, "pipeline_runs",
    data.frame(
      run_id = run_id,
      mode = cfg$project$mode,
      started_at = as.character(Sys.time()),
      finished_at = as.character(Sys.time()),
      config_snapshot = jsonlite::toJSON(cfg$scoring, auto_unbox = TRUE),
      status = "ok",
      notes = "scripts/06_generate_rankings.R",
      stringsAsFactors = FALSE
    ),
    append = TRUE
  )
}

write_log("Rankings complete. run_id=", run_id)
