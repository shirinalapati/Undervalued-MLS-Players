# Generate a reproducible demo cohort for offline / portfolio runs.
# Synthetic players are labeled clearly and are not real athletes.

generate_demo_cohort <- function(cfg, n_players = 180, seed = NULL, output_dir = NULL) {
  ensure_packages(c("dplyr", "tibble", "readr", "jsonlite"))
  seed <- seed %||% cfg$project$random_seed %||% 42
  set.seed(seed)

  demo_dir <- dir_create_safe(output_dir %||% cfg$paths$demo)
  leagues <- c("mls", "mlsnp", "uslc")
  positions <- c("FW", "W", "CM", "FB", "CB")
  role_map <- c(
    FW = "pressing_striker",
    W = "transition_winger",
    CM = "ball_winning_midfielder",
    CB = "progressive_center_back",
    FB = "overlapping_fullback"
  )

  first <- c("Alex","Jordan","Sam","Casey","Riley","Morgan","Avery","Quinn","Parker","Reese",
             "Cameron","Drew","Jamie","Taylor","Rowan","Skyler","Emerson","Hayden","Finley","Blake")
  last <- c("Nguyen","Patel","Garcia","Schmidt","Okoye","Silva","Andersen","Kowalski","Mensah","Ivanov",
            "Costa","Berg","Nakamura","Hassan","Okafor","Petrov","Dubois","Johansson","Ali","Brooks")

  players <- tibble::tibble(
    player_id = sprintf("demo_%03d", seq_len(n_players)),
    display_name = paste(sample(first, n_players, TRUE), sample(last, n_players, TRUE)),
    league_id = sample(leagues, n_players, TRUE, prob = c(0.35, 0.30, 0.35)),
    position_group = sample(positions, n_players, TRUE),
    age = round(runif(n_players, 18, 33), 1),
    is_domestic_player = as.integer(runif(n_players) < 0.55),
    intl_roster_status = NA_character_, # unknown in demo unless set
    season_year = cfg$project$product_season %||% cfg$project$season %||% 2026
  ) |>
    dplyr::mutate(
      normalized_name = normalize_player_name(display_name),
      tactical_role = unname(role_map[position_group]),
      minutes = pmax(200, round(rnorm(n_players, 1800, 600))),
      team_strength = rnorm(n_players, 0, 1)
    )

  # Latent talent + role traits
  n_row <- nrow(players)
  players <- players |>
    dplyr::mutate(
      latent = rnorm(n_row, 0, 1),
      press = clip(latent + rnorm(n_row, 0, 0.7) + dplyr::if_else(position_group %in% c("FW", "CM"), 0.3, 0), -3, 3),
      progress = clip(latent + rnorm(n_row, 0, 0.7), -3, 3),
      create = clip(latent + rnorm(n_row, 0, 0.8), -3, 3),
      finish = clip(latent + rnorm(n_row, 0, 0.8) + dplyr::if_else(position_group == "FW", 0.4, 0), -3, 3),
      defend = clip(rnorm(n_row, 0, 1) + dplyr::if_else(position_group %in% c("CB", "CM", "FB"), 0.4, -0.2), -3, 3)
    )

  # League quality inflation for counting stats
  league_boost <- c(mls = 0.15, mlsnp = -0.25, uslc = -0.10)

  stats <- players |>
    dplyr::mutate(
      boost = unname(league_boost[league_id]),
      npxg_p90 = pmax(0.01, 0.25 + 0.12 * finish + 0.05 * boost + rnorm(n_row, 0, 0.05)),
      xa_p90 = pmax(0.01, 0.15 + 0.10 * create + 0.04 * boost + rnorm(n_row, 0, 0.04)),
      shots_p90 = pmax(0.2, 1.8 + 0.6 * finish + rnorm(n_row, 0, 0.3)),
      pressures_p90 = pmax(1, 12 + 3 * press + rnorm(n_row, 0, 2)),
      tackles_p90 = pmax(0.2, 1.5 + 0.6 * defend + rnorm(n_row, 0, 0.3)),
      interceptions_p90 = pmax(0.2, 1.2 + 0.5 * defend + rnorm(n_row, 0, 0.25)),
      progressive_passes_p90 = pmax(0.5, 4 + 1.2 * progress + rnorm(n_row, 0, 0.6)),
      progressive_carries_p90 = pmax(0.3, 3 + 1.0 * progress + rnorm(n_row, 0, 0.5)),
      goals_added_p90 = pmax(-0.05, 0.08 + 0.04 * latent + rnorm(n_row, 0, 0.02)),
      aerial_win_pct = clip(0.45 + 0.08 * defend + rnorm(n_row, 0, 0.07), 0.15, 0.85),
      pass_completion_pct = clip(0.78 + 0.04 * progress + rnorm(n_row, 0, 0.04), 0.55, 0.95),
      crosses_p90 = pmax(0, 1.2 + dplyr::if_else(position_group == "FB", 1.5, 0) + rnorm(n_row, 0, 0.5)),
      # YoY improvement signal for development
      yoy_delta = rnorm(n_row, 0.02, 0.08) - pmax(0, (age - 26) * 0.01)
    )

  # Cost: younger + lower league cheaper; MLS stars expensive
  sal_draw <- dplyr::case_when(
    stats$league_id == "mls" ~ pmax(80000, round(exp(rnorm(n_row, log(350000), 0.7)))),
    stats$league_id == "uslc" ~ pmax(30000, round(exp(rnorm(n_row, log(70000), 0.5)))),
    TRUE ~ pmax(20000, round(exp(rnorm(n_row, log(45000), 0.5))))
  )
  stats <- stats |>
    dplyr::mutate(
      salary = sal_draw,
      cost_tier = dplyr::case_when(
        salary < 150000 & league_id != "mls" ~ 1L,
        salary < 300000 ~ 2L,
        salary < 700000 ~ 3L,
        salary < 1500000 ~ 4L,
        TRUE ~ 5L
      ),
      compensation_known = TRUE,
      guaranteed_compensation_tier = cost_tier,
      financial_data_confidence = "demo_synthetic_compensation",
      minutes_share = clip(minutes / 3060, 0.05, 1)
    )

  teams <- tibble::tibble(
    team_id = sprintf("%s_team_%02d", rep(leagues, each = 8), rep(1:8, 3)),
    team_name = paste(rep(c("Harbor","Summit","River","Metro","Canyon","Prairie","Bay","Capital"), 3),
                      rep(c("FC","SC","United","Athletic"), length.out = 24)),
    league_id = rep(leagues, each = 8),
    is_mls_club = as.integer(rep(leagues, each = 8) == "mls")
  )

  stats$team_id <- sample(teams$team_id[teams$league_id == stats$league_id[1]], n_players, TRUE)
  # assign team within league
  stats <- stats |>
    dplyr::rowwise() |>
    dplyr::mutate(team_id = sample(teams$team_id[teams$league_id == league_id], 1)) |>
    dplyr::ungroup()

  readr::write_csv(stats, file.path(demo_dir, "demo_player_season.csv"))
  readr::write_csv(teams, file.path(demo_dir, "demo_teams.csv"))

  meta <- list(
    generated_at = as.character(Sys.time()),
    seed = seed,
    n_players = n_players,
    note = "Synthetic demo players for offline pipeline — not real athletes."
  )
  jsonlite::write_json(meta, file.path(demo_dir, "demo_meta.json"), pretty = TRUE, auto_unbox = TRUE)

  write_log("Wrote demo cohort to ", demo_dir)
  invisible(list(players = stats, teams = teams))
}

`%||%` <- function(a, b) if (!is.null(a)) a else b
