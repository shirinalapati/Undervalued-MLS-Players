#!/usr/bin/env Rscript
# 00_generate_demo_data.R

source("R/utilities/load_project.R")
source("R/collect/demo_generate.R")

cfg <- load_config()
set.seed(cfg$project$random_seed)
invisible(generate_demo_cohort(cfg, n_players = 180))
write_log("Demo data generation complete.")
