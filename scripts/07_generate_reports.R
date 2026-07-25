#!/usr/bin/env Rscript
# 07_generate_reports.R — Excel + summary exports for MLS Value Index

source("R/utilities/load_project.R")
source("R/utilities/data_provenance.R")
source("R/models/value_index.R")
source("R/exports/value_excel.R")

cfg <- load_config()
vi <- load_value_index_config()
ensure_packages(c("readr", "dplyr"))
prov <- read_provenance(cfg)

if (isTRUE(prov$is_synthetic) && identical(cfg$project$mode, "live")) {
  stop("Refusing live exports from synthetic data.")
}

scores <- readr::read_csv(
  file.path(cfg$paths$processed, "player_value_scores_blended.csv"),
  show_col_types = FALSE
)

dir_create_safe(cfg$paths$exports)
xlsx_path <- file.path(cfg$paths$exports, "MLS_Value_Index_2026.xlsx")
export_value_index_xlsx(scores, xlsx_path, vi_cfg = vi, provenance = prov)
write_log("Wrote ", xlsx_path)

official <- scores |> dplyr::filter(official_eligible) |> dplyr::arrange(dplyr::desc(undervaluation_score))
readr::write_csv(official, file.path(cfg$paths$exports, "mls_value_index_official_rankings.csv"))
write_log("Wrote exports/mls_value_index_official_rankings.csv (", nrow(official), " players)")
