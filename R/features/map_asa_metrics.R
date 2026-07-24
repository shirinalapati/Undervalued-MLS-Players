# Map ASA API payloads into the project's standardized player-season schema.

# MLSPA/ASA compensation is USD; convert any non-USD source amounts to USD.
.FX_TO_USD <- c(USD = 1, CAD = 0.74, EUR = 1.08, GBP = 1.27, MXN = 0.055)
to_salary_usd <- function(amount, currency = "USD") {
  amount <- as.numeric(amount)
  currency <- toupper(trimws(as.character(currency)))
  currency[is.na(currency) | !nzchar(currency)] <- "USD"
  rate <- .FX_TO_USD[currency]
  rate[!is.finite(rate)] <- 1
  amount * as.numeric(rate)
}

pick_col <- function(df, candidates, default = NA) {
  hit <- candidates[candidates %in% names(df)]
  if (!length(hit)) return(rep(default, nrow(df)))
  df[[hit[[1]]]]
}

unnest_goals_added <- function(gplus) {
  if (is.null(gplus) || !nrow(gplus)) return(NULL)
  ensure_packages(c("dplyr", "tidyr"))

  if ("data" %in% names(gplus)) {
    wide <- tryCatch({
      long <- gplus |> tidyr::unnest(data)
      # ASA nested g+ uses goals_added_raw; older shapes may use goals_added
      if ("goals_added_raw" %in% names(long) && !"goals_added" %in% names(long)) {
        long$goals_added <- long$goals_added_raw
      }
      if (!"goals_added" %in% names(long)) {
        stop("No goals_added / goals_added_raw column after unnest")
      }
      # ASA defending family is labeled "Interrupting"
      long$action_family <- dplyr::case_when(
        long$action_type %in% c("Defending", "Interrupting") ~ "defending",
        long$action_type == "Dribbling" ~ "dribbling",
        long$action_type == "Passing" ~ "passing",
        long$action_type == "Receiving" ~ "receiving",
        long$action_type == "Shooting" ~ "shooting",
        long$action_type == "Fouling" ~ "fouling",
        TRUE ~ tolower(as.character(long$action_type))
      )
      long |>
        dplyr::group_by(player_id, dplyr::across(dplyr::any_of(c("team_id", "season_name", "general_position")))) |>
        dplyr::summarise(
          goals_added_total = sum(goals_added, na.rm = TRUE),
          goals_added_dribbling = sum(goals_added[action_family == "dribbling"], na.rm = TRUE),
          goals_added_passing = sum(goals_added[action_family == "passing"], na.rm = TRUE),
          goals_added_receiving = sum(goals_added[action_family == "receiving"], na.rm = TRUE),
          goals_added_shooting = sum(goals_added[action_family == "shooting"], na.rm = TRUE),
          goals_added_defending = sum(goals_added[action_family == "defending"], na.rm = TRUE),
          goals_added_fouling = sum(goals_added[action_family == "fouling"], na.rm = TRUE),
          .groups = "drop"
        )
    }, error = function(e) {
      write_log("unnest_goals_added failed: ", conditionMessage(e))
      NULL
    })
    return(wide)
  }

  gplus
}

map_general_position <- function(x) {
  x <- toupper(as.character(x))
  dplyr::case_when(
    x %in% c("ST", "CF", "FW", "F") ~ "FW",
    x %in% c("W", "LW", "RW", "AM", "CAM", "M/F") ~ "W",
    x %in% c("CM", "DM", "CDM", "M", "MF") ~ "CM",
    x %in% c("FB", "LB", "RB", "LWB", "RWB", "D/M") ~ "FB",
    x %in% c("CB", "LCB", "RCB", "D") ~ "CB",
    x %in% c("GK", "G") ~ "GK",
    TRUE ~ "CM"
  )
}

primary_role_for_position <- function(pos) {
  dplyr::case_when(
    pos == "FW" ~ "pressing_striker",
    pos == "W" ~ "transition_winger",
    pos == "CM" ~ "ball_winning_midfielder",
    pos == "CB" ~ "progressive_center_back",
    pos == "FB" ~ "overlapping_fullback",
    TRUE ~ "ball_winning_midfielder"
  )
}

estimate_age_from_birthdate <- function(birth_date, season_year) {
  bd <- as.Date(birth_date)
  ref <- as.Date(paste0(season_year, "-07-01"))
  as.numeric(difftime(ref, bd, units = "days")) / 365.25
}

flatten_asa_collection <- function(collection) {
  ensure_packages(c("dplyr", "purrr", "tidyr", "tibble"))

  purrr::map_dfr(collection, function(payload) {
    if (is.null(payload$xgoals) || !is.data.frame(payload$xgoals) || !nrow(payload$xgoals)) {
      return(NULL)
    }

    xg <- payload$xgoals
    xp <- payload$xpass
    gp <- unnest_goals_added(payload$goals_added)
    sal <- payload$salaries
    plist <- payload$players
    tlist <- payload$teams

    base <- tibble::tibble(
      asa_player_id = as.character(pick_col(xg, c("player_id"))),
      team_id_raw = as.character(pick_col(xg, c("team_id"))),
      season_year = as.integer(payload$season),
      league_id = payload$league,
      minutes = as.numeric(pick_col(xg, c("minutes_played", "minutes", "min"))),
      npxg = as.numeric(pick_col(xg, c("xgoals", "npxgoals", "xG", "xg"))),
      shots = as.numeric(pick_col(xg, c("shots", "shots_for"))),
      goals = as.numeric(pick_col(xg, c("goals", "goal"))),
      general_position = as.character(pick_col(xg, c("general_position", "position", "primary_position"), default = NA_character_))
    )

    if (!is.null(xp) && is.data.frame(xp) && nrow(xp) && "player_id" %in% names(xp)) {
      xp2 <- tibble::tibble(
        asa_player_id = as.character(xp$player_id),
        team_id_raw = as.character(pick_col(xp, c("team_id"))),
        xa = as.numeric(pick_col(xp, c("xassists", "xa", "key_passes", "xPass"))),
        xpass_diff = as.numeric(pick_col(xp, c("score_diff", "xpass_diff", "passes_completed_over_expected")))
      )
      # Prefer join on player+team when possible
      if (all(!is.na(base$team_id_raw)) && all(!is.na(xp2$team_id_raw))) {
        base <- dplyr::left_join(base, xp2, by = c("asa_player_id", "team_id_raw"))
      } else {
        xp2 <- xp2 |>
          dplyr::group_by(asa_player_id) |>
          dplyr::summarise(xa = mean(xa, na.rm = TRUE), xpass_diff = mean(xpass_diff, na.rm = TRUE), .groups = "drop")
        base <- dplyr::left_join(base, xp2, by = "asa_player_id")
      }
    } else {
      base$xa <- NA_real_
      base$xpass_diff <- NA_real_
    }

    if (!is.null(gp) && is.data.frame(gp) && nrow(gp) && "player_id" %in% names(gp)) {
      g_total <- if ("goals_added_total" %in% names(gp)) {
        gp$goals_added_total
      } else if ("goals_added" %in% names(gp)) {
        gp$goals_added
      } else {
        rep(NA_real_, nrow(gp))
      }
      gp2 <- tibble::tibble(
        asa_player_id = as.character(gp$player_id),
        goals_added = as.numeric(g_total),
        goals_added_defending = as.numeric(if ("goals_added_defending" %in% names(gp)) gp$goals_added_defending else NA_real_),
        goals_added_passing = as.numeric(if ("goals_added_passing" %in% names(gp)) gp$goals_added_passing else NA_real_),
        goals_added_dribbling = as.numeric(if ("goals_added_dribbling" %in% names(gp)) gp$goals_added_dribbling else NA_real_),
        goals_added_receiving = as.numeric(if ("goals_added_receiving" %in% names(gp)) gp$goals_added_receiving else NA_real_),
        goals_added_shooting = as.numeric(if ("goals_added_shooting" %in% names(gp)) gp$goals_added_shooting else NA_real_),
        goals_added_fouling = as.numeric(if ("goals_added_fouling" %in% names(gp)) gp$goals_added_fouling else NA_real_)
      ) |>
        dplyr::group_by(asa_player_id) |>
        dplyr::summarise(dplyr::across(dplyr::where(is.numeric), \(x) sum(x, na.rm = TRUE)), .groups = "drop")
      base <- dplyr::left_join(base, gp2, by = "asa_player_id")
    } else {
      base$goals_added <- NA_real_
      base$goals_added_defending <- NA_real_
      base$goals_added_passing <- NA_real_
      base$goals_added_dribbling <- NA_real_
      base$goals_added_receiving <- NA_real_
      base$goals_added_shooting <- NA_real_
      base$goals_added_fouling <- NA_real_
    }

    if (!is.null(plist) && is.data.frame(plist) && nrow(plist)) {
      nm <- pick_col(plist, c("player_name", "player_known_name"), default = NA_character_)
      if (all(is.na(nm))) {
        fn <- pick_col(plist, c("player_first_name", "first_name"), default = "")
        ln <- pick_col(plist, c("player_last_name", "last_name"), default = "")
        nm <- trimws(paste(fn, ln))
      }
      p2 <- tibble::tibble(
        asa_player_id = as.character(plist$player_id),
        display_name = as.character(nm),
        birth_date = as.character(pick_col(plist, c("birth_date", "birthday", "date_of_birth"), default = NA_character_)),
        nationality = as.character(pick_col(plist, c("nationality", "nation", "country"), default = NA_character_))
      ) |>
        dplyr::distinct(asa_player_id, .keep_all = TRUE)
      base <- dplyr::left_join(base, p2, by = "asa_player_id")
    } else {
      base$display_name <- base$asa_player_id
      base$birth_date <- NA_character_
      base$nationality <- NA_character_
    }

    if (!is.null(tlist) && is.data.frame(tlist) && nrow(tlist)) {
      t2 <- tibble::tibble(
        team_id_raw = as.character(tlist$team_id),
        team_name = as.character(pick_col(tlist, c("team_name", "team_abbreviation"), default = as.character(tlist$team_id)))
      ) |>
        dplyr::distinct(team_id_raw, .keep_all = TRUE)
      base <- dplyr::left_join(base, t2, by = "team_id_raw")
    } else {
      base$team_name <- base$team_id_raw
    }

    if (!is.null(sal) && is.data.frame(sal) && nrow(sal) && "player_id" %in% names(sal)) {
      # MLSPA salary guide via ASA is denominated in USD (no currency column in feed).
      raw_currency <- if ("currency" %in% names(sal)) as.character(sal$currency) else "USD"
      s2 <- tibble::tibble(
        asa_player_id = as.character(sal$player_id),
        salary_native = as.numeric(pick_col(sal, c("guaranteed_compensation", "base_salary", "salary"))),
        salary_currency = toupper(trimws(dplyr::coalesce(raw_currency, "USD")))
      ) |>
        dplyr::mutate(
          salary_currency = dplyr::if_else(
            is.na(salary_currency) | !nzchar(salary_currency), "USD", salary_currency
          ),
          salary = to_salary_usd(salary_native, salary_currency)
        ) |>
        dplyr::group_by(asa_player_id) |>
        dplyr::summarise(
          salary = suppressWarnings(max(salary, na.rm = TRUE)),
          salary_currency = "USD",
          .groups = "drop"
        )
      s2$salary[!is.finite(s2$salary)] <- NA_real_
      base <- dplyr::left_join(base, s2, by = "asa_player_id")
    } else {
      base$salary <- NA_real_
      base$salary_currency <- "USD"
    }

    base$retrieved_at <- payload$retrieved_at
    base
  })
}

standardize_asa_players <- function(flat, cfg) {
  ensure_packages(c("dplyr"))
  product_season <- cfg$project$product_season %||% 2026

  # Ensure g+ component columns exist before rate construction
  for (gc in c("goals_added", "goals_added_defending", "goals_added_passing",
               "goals_added_dribbling", "goals_added_receiving", "goals_added_shooting",
               "goals_added_fouling")) {
    if (!gc %in% names(flat)) flat[[gc]] <- NA_real_
  }

  # Keep all 2024–2026 rows for YTD / baseline / development construction
  flat <- flat |>
    dplyr::filter(!is.na(asa_player_id), !is.na(minutes), minutes > 0) |>
    dplyr::filter(season_year %in% c(2024, 2025, 2026))

  flat <- flat |>
    dplyr::mutate(
      position_group = map_general_position(general_position),
      age = dplyr::coalesce(
        estimate_age_from_birthdate(birth_date, season_year),
        estimate_age_from_birthdate(birth_date, product_season),
        dplyr::case_when(
          league_id == "mlsnp" ~ 21,
          league_id == "uslc" ~ 25,
          TRUE ~ 26
        )
      ),
      display_name = trimws(dplyr::coalesce(display_name, asa_player_id)),
      normalized_name = normalize_player_name(display_name),
      player_id = paste0("asa_", asa_player_id, "_", league_id, "_", season_year),
      team_id = dplyr::coalesce(team_id_raw, paste0(league_id, "_unknown")),
      # Nationality is NOT MLS international-roster status
      nationality_hint_usa_can = as.integer(grepl("USA|United States|CAN|Canada", nationality, ignore.case = TRUE)),
      is_domestic_player = NA_integer_, # deprecated inference — prefer intl_roster_status
      intl_roster_status = NA_character_, # Unknown until official roster snapshot
      npxg_p90 = dplyr::if_else(minutes > 0, dplyr::coalesce(npxg, 0) * 90 / minutes, NA_real_),
      xa_p90 = dplyr::if_else(minutes > 0, dplyr::coalesce(xa, 0) * 90 / minutes, NA_real_),
      shots_p90 = dplyr::if_else(minutes > 0, dplyr::coalesce(shots, 0) * 90 / minutes, NA_real_),
      # Keep NA when g+ missing — do not coalesce to 0 (fabricates zero contribution)
      goals_added_p90 = dplyr::if_else(
        minutes > 0 & is.finite(goals_added),
        goals_added * 90 / minutes,
        NA_real_
      ),
      goals_added_shooting_p90 = dplyr::if_else(minutes > 0 & is.finite(goals_added_shooting), goals_added_shooting * 90 / minutes, NA_real_),
      goals_added_receiving_p90 = dplyr::if_else(minutes > 0 & is.finite(goals_added_receiving), goals_added_receiving * 90 / minutes, NA_real_),
      goals_added_passing_p90 = dplyr::if_else(minutes > 0 & is.finite(goals_added_passing), goals_added_passing * 90 / minutes, NA_real_),
      goals_added_dribbling_p90 = dplyr::if_else(minutes > 0 & is.finite(goals_added_dribbling), goals_added_dribbling * 90 / minutes, NA_real_),
      goals_added_defending_p90 = dplyr::if_else(minutes > 0 & is.finite(goals_added_defending), goals_added_defending * 90 / minutes, NA_real_),
      goals_added_fouling_p90 = dplyr::if_else(minutes > 0 & is.finite(goals_added_fouling), goals_added_fouling * 90 / minutes, NA_real_),
      # g+-derived PROXY rates (not ASA native pressures/tackles fields) — labeled in model_spec
      pressures_p90 = dplyr::if_else(
        minutes > 0 & is.finite(goals_added_defending),
        goals_added_defending * 90 / minutes * 40 + 8,
        NA_real_
      ),
      tackles_p90 = dplyr::if_else(
        minutes > 0 & is.finite(goals_added_defending),
        goals_added_defending * 90 / minutes * 8 + 1.2,
        NA_real_
      ),
      interceptions_p90 = dplyr::if_else(
        minutes > 0 & is.finite(goals_added_defending),
        goals_added_defending * 90 / minutes * 6 + 1.0,
        NA_real_
      ),
      progressive_passes_p90 = dplyr::if_else(
        minutes > 0 & is.finite(goals_added_passing),
        goals_added_passing * 90 / minutes * 25 + 3,
        NA_real_
      ),
      progressive_carries_p90 = dplyr::if_else(
        minutes > 0 & is.finite(goals_added_dribbling),
        goals_added_dribbling * 90 / minutes * 20 + 2.5,
        NA_real_
      ),
      aerial_win_pct = NA_real_, # not fabricated
      pass_completion_pct = dplyr::coalesce(0.78 + pmin(pmax(xpass_diff, -0.1), 0.1), NA_real_),
      crosses_p90 = NA_real_, # not fabricated by position
      yoy_delta = 0,
      salary_currency = "USD",
      salary_source = dplyr::if_else(is.finite(salary), "mlspa_via_asa", NA_character_),
      salary_as_of = dplyr::if_else(is.finite(salary), as.character(cfg$acquisition$asa$salary_as_of %||% NA_character_), NA_character_),
      compensation_known = is.finite(salary),
      # Do not fabricate non-MLS compensation tiers
      salary = dplyr::if_else(is.finite(salary), to_salary_usd(salary, dplyr::coalesce(salary_currency, "USD")), NA_real_),
      salary_usd = salary,
      cost_tier = dplyr::case_when(
        !is.finite(salary) ~ NA_integer_,
        salary < 150000 & league_id != "mls" ~ 1L,
        salary < 300000 ~ 2L,
        salary < 700000 ~ 3L,
        salary < 1500000 ~ 4L,
        TRUE ~ 5L
      ),
      guaranteed_compensation_tier = cost_tier,
      estimated_acquisition_cost_tier = NA_integer_, # not modeled
      estimated_roster_budget_complexity = NA_integer_, # not modeled
      financial_data_confidence = dplyr::if_else(is.finite(salary), "known_guaranteed_compensation", "unknown"),
      minutes_share = pmin(pmax(minutes / 3060, 0.05), 1),
      tactical_role_primary = primary_role_for_position(position_group),
      data_source = "asa_live",
      season_status = dplyr::case_when(
        season_year == 2026 ~ "ytd_in_progress",
        season_year == 2025 ~ "full_completed",
        season_year == 2024 ~ "historical_trend_only",
        TRUE ~ "other"
      )
    ) |>
    dplyr::filter(position_group != "GK")

  # Propagate newest known salary onto older seasons by asa_player_id (do not invent)
  salary_map <- flat |>
    dplyr::filter(is.finite(salary)) |>
    dplyr::group_by(asa_player_id) |>
    dplyr::slice_max(order_by = season_year, n = 1, with_ties = FALSE) |>
    dplyr::ungroup() |>
    dplyr::select(asa_player_id, salary_latest = salary, cost_tier_latest = cost_tier,
                  compensation_known_latest = compensation_known)

  flat <- flat |>
    dplyr::left_join(salary_map, by = "asa_player_id") |>
    dplyr::mutate(
      salary = dplyr::coalesce(salary_latest, salary),
      cost_tier = dplyr::coalesce(cost_tier_latest, cost_tier),
      compensation_known = dplyr::coalesce(compensation_known_latest, compensation_known),
      guaranteed_compensation_tier = cost_tier,
      financial_data_confidence = dplyr::if_else(
        is.finite(salary), "known_guaranteed_compensation", "unknown"
      )
    ) |>
    dplyr::select(-dplyr::any_of(c("salary_latest", "cost_tier_latest", "compensation_known_latest")))

  teams <- flat |>
    dplyr::distinct(team_id, team_name, league_id) |>
    dplyr::transmute(
      team_id,
      team_name = dplyr::coalesce(team_name, team_id),
      team_short = team_name,
      league_id,
      is_mls_club = as.integer(league_id == "mls"),
      conference = NA_character_,
      asa_team_id = team_id
    ) |>
    dplyr::distinct(team_id, .keep_all = TRUE)

  players <- flat |>
    dplyr::transmute(
      player_id, display_name, normalized_name, birth_date, nationality,
      primary_position = position_group, is_domestic_player, intl_roster_status, nationality_hint_usa_can,
      asa_player_id,
      age, league_id, team_id, season_year, season_status, position_group, tactical_role_primary,
      minutes, npxg_p90, xa_p90, shots_p90, pressures_p90, tackles_p90, interceptions_p90,
      progressive_passes_p90, progressive_carries_p90, goals_added_p90,
      goals_added_shooting_p90, goals_added_receiving_p90, goals_added_passing_p90,
      goals_added_dribbling_p90, goals_added_defending_p90, goals_added_fouling_p90,
      aerial_win_pct,
      pass_completion_pct, crosses_p90, yoy_delta,
      salary, salary_usd, salary_currency, salary_source, salary_as_of, compensation_known,
      cost_tier, guaranteed_compensation_tier, estimated_acquisition_cost_tier,
      estimated_roster_budget_complexity, financial_data_confidence, minutes_share,
      data_source
    )

  list(players = players, teams = teams)
}
