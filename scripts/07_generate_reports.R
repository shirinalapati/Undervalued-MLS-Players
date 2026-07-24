#!/usr/bin/env Rscript
# 07_generate_reports.R

source("R/utilities/load_project.R")
source("R/utilities/data_provenance.R")

cfg <- load_config()
ensure_packages(c("readr", "dplyr", "rmarkdown"))
prov <- read_provenance(cfg)

scored <- readr::read_csv(file.path(cfg$paths$processed, "player_component_scores.csv"), show_col_types = FALSE)
case_path <- file.path(cfg$paths$processed, "rankings_case_studies.csv")
case_rankings <- if (file.exists(case_path)) {
  readr::read_csv(case_path, show_col_types = FALSE)
} else {
  dplyr::tibble()
}

dir_create_safe(file.path(PROJECT_ROOT, "reports", "_output"))
dir_create_safe(cfg$paths$exports)

if (isTRUE(prov$is_synthetic)) {
  write_log("SYNTHETIC DEMO DATA — writing DEMO-ONLY exports; blocking genuine scouting-report labeling.")
  if (nrow(case_rankings)) {
    demo <- case_rankings |>
      dplyr::mutate(
        display_name = paste0("[DEMO] ", display_name),
        export_disclaimer = "SYNTHETIC DEMO DATA — NOT A GENUINE SCOUTING REPORT",
        data_cutoff_label = cutoff_label(prov)
      )
    readr::write_csv(demo, file.path(cfg$paths$exports, "DEMO_ONLY_shortlist_memo.csv"))
    # Remove any prior unwatermarked memo
    genuine <- file.path(cfg$paths$exports, "shortlist_memo.csv")
    if (file.exists(genuine)) file.remove(genuine)
    write_log("Wrote exports/DEMO_ONLY_shortlist_memo.csv")
  }
  write_log("Skipping genuine HTML scouting reports while synthetic data is loaded.")
  quit(save = "no", status = 0)
}

# Live path
if (nrow(case_rankings)) {
  top <- case_rankings |> dplyr::slice_head(n = 1)
  player <- scored |>
    dplyr::filter(player_id == top$player_id[[1]], role_id == top$role_id[[1]]) |>
    dplyr::slice_head(n = 1)

  params_path <- file.path(cfg$paths$processed, "report_params_player.rds")
  saveRDS(
    list(
      player = player,
      shortlist = case_rankings,
      cfg = cfg,
      provenance = prov,
      cutoff_label = cutoff_label(prov)
    ),
    params_path
  )

  player_rmd <- file.path(PROJECT_ROOT, "reports", "player_report.Rmd")
  shortlist_rmd <- file.path(PROJECT_ROOT, "reports", "shortlist_report.Rmd")

  if (file.exists(player_rmd) && rmarkdown::pandoc_available()) {
    rmarkdown::render(
      player_rmd,
      output_dir = file.path(PROJECT_ROOT, "reports", "_output"),
      params = list(params_rds = params_path),
      quiet = TRUE
    )
    write_log("Rendered player report.")
  }

  if (file.exists(shortlist_rmd) && rmarkdown::pandoc_available()) {
    rmarkdown::render(
      shortlist_rmd,
      output_dir = file.path(PROJECT_ROOT, "reports", "_output"),
      params = list(params_rds = params_path),
      quiet = TRUE
    )
    write_log("Rendered shortlist memo.")
  }

  export_df <- case_rankings |>
    dplyr::group_by(club_id) |>
    dplyr::slice_head(n = 15) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      data_cutoff_label = cutoff_label(prov),
      export_disclaimer = paste(
        "Live shortlist.",
        cutoff_label(prov),
        "2026 statistics are season-to-date unless evaluation period is 2025 full season."
      )
    )
  readr::write_csv(export_df, file.path(cfg$paths$exports, "shortlist_memo.csv"))
  write_log("Wrote exports/shortlist_memo.csv | ", cutoff_label(prov))
} else {
  write_log("No case rankings found; skip report render.")
}

write_log("Report generation step finished.")
