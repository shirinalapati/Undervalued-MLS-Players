# Club roster inference + club × position × tactical-role needs model.
# Public-data planning signals — not official club priorities or replace-player orders.

POSITION_CHOICES <- c(
  "Any" = "any",
  "Forward (FW)" = "FW",
  "Winger (W)" = "W",
  "Central Midfielder (CM)" = "CM",
  "Fullback (FB)" = "FB",
  "Center Back (CB)" = "CB"
)

RECRUITMENT_OBJECTIVES <- c(
  "Direct replacement" = "direct_replacement",
  "Upgrade" = "upgrade",
  "Rotation depth" = "rotation_depth",
  "Younger successor" = "younger_successor",
  "Lower-cost alternative" = "lower_cost",
  "Complementary profile" = "complementary"
)

# Adjacent positions that can contribute partial depth to a role's position group
POSITION_FLEX <- list(
  FW = c("FW", "W"),
  W = c("W", "FW", "FB"),
  CM = c("CM", "W", "FB"),
  FB = c("FB", "W", "CB"),
  CB = c("CB", "FB")
)

DEFAULT_NEED_WEIGHTS <- list(
  starter_quality_gap = 0.30,
  effective_depth_gap = 0.25,
  succession_risk = 0.15,
  availability_risk = 0.10,
  tactical_coverage_gap = 0.10,
  financial_efficiency_opportunity = 0.10
)

need_weights_from_cfg <- function(cfg = NULL) {
  w <- DEFAULT_NEED_WEIGHTS
  if (!is.null(cfg$roster_needs$weights)) {
    for (nm in names(cfg$roster_needs$weights)) {
      if (nm %in% names(w)) w[[nm]] <- as.numeric(cfg$roster_needs$weights[[nm]])
    }
  }
  s <- sum(unlist(w))
  if (s > 0) w <- lapply(w, function(x) x / s)
  w
}

load_club_team_map <- function() {
  load_yaml("config/club_team_map.yml")
}

asa_team_ids_for_club <- function(club_id, team_map = NULL) {
  team_map <- team_map %||% load_club_team_map()
  entry <- team_map$clubs[[club_id]]
  if (is.null(entry)) return(character())
  as.character(unlist(entry$asa_team_ids %||% list()))
}

club_id_for_team <- function(team_id, team_map = NULL) {
  team_map <- team_map %||% load_club_team_map()
  for (cid in names(team_map$clubs)) {
    ids <- as.character(unlist(team_map$clubs[[cid]]$asa_team_ids %||% list()))
    if (team_id %in% ids) return(cid)
  }
  NA_character_
}

# club_roster_players() lives in R/models/current_rosters.R (2026 salary guide + active minutes)

#' Roster rows for a position + preferred tactical role (one row per player at that role).
roster_for_role <- function(players_df, club_id, role_id, position = "any",
                            team_map = NULL, active_only = TRUE) {
  ensure_packages("dplyr")
  rost <- club_roster_players(players_df, club_id, team_map, active_only = active_only)
  if (!nrow(rost)) return(rost)

  pos <- if (identical(position, "any") || is.null(position) || !nzchar(position)) {
    ROLE_POSITION_MAP[[role_id]] %||% NA_character_
  } else {
    position
  }

  # Prefer players whose primary position matches; fall back to full active roster
  if (!is.na(pos) && nzchar(pos) && "position_group" %in% names(rost)) {
    matched <- dplyr::filter(rost, .data$position_group == .env$pos)
    if (nrow(matched)) rost <- matched
  }

  # If role_id column exists (from scores join), prefer that role's scoring row via players_df
  if (!is.null(players_df) && "role_id" %in% names(players_df) && nrow(rost)) {
    role_rows <- players_df |>
      dplyr::filter(.data$role_id == .env$role_id, .data$asa_player_id %in% rost$asa_player_id) |>
      dplyr::group_by(.data$asa_player_id) |>
      dplyr::slice_head(n = 1) |>
      dplyr::ungroup()
    if (nrow(role_rows)) {
      # Keep roster membership/status; refresh role-specific scores
      keep_meta <- c("asa_player_id", "club_id", "team_id", "roster_status", "minutes_2026", "salary_usd")
      keep_meta <- intersect(keep_meta, names(rost))
      meta <- rost[, keep_meta, drop = FALSE]
      out <- dplyr::left_join(meta, role_rows, by = "asa_player_id")
      return(dplyr::arrange(out, dplyr::desc(dplyr::coalesce(.data$minutes_2026, .data$minutes, 0))))
    }
  }

  dplyr::arrange(rost, dplyr::desc(dplyr::coalesce(.data$minutes_2026, .data$minutes, 0)))
}

# ---- Effective depth & slot metrics ------------------------------------------------

clip01 <- function(x) pmin(1, pmax(0, as.numeric(x)))
clip100 <- function(x) pmin(100, pmax(0, as.numeric(x)))

#' How much a player contributes to effective depth in a tactical role.
player_role_depth_weight <- function(player_row, role_id, position_group, cfg = NULL) {
  role_fit <- as.numeric(player_row$score_role_fit[[1]] %||% 50) / 100
  proj <- as.numeric(player_row$score_projected_mls[[1]] %||% 50) / 100

  mins <- as.numeric(player_row$minutes[[1]] %||% 0)
  # Sample reliability: more minutes → more trust; YTD blends raise weight with minutes
  m0 <- cfg$project$blend$prior_strength_minutes %||% 700
  sample_rel <- clip01(mins / (mins + m0))
  # Soft floor so low-minute depth pieces still count a little
  sample_rel <- 0.25 + 0.75 * sample_rel

  # Availability proxy from model risk (public data — not medical/contract)
  risk <- as.numeric(player_row$score_risk[[1]] %||% 40)
  availability <- clip01(1 - (risk / 120))

  # Expected minutes capacity: share of a full season workload
  minutes_capacity <- clip01(mins / 2500)
  minutes_capacity <- 0.35 + 0.65 * minutes_capacity

  # Positional flexibility
  pg <- as.character(player_row$position_group[[1]] %||% "")
  flex_set <- POSITION_FLEX[[position_group]] %||% position_group
  if (identical(pg, position_group)) {
    flex <- 1.0
  } else if (pg %in% flex_set) {
    flex <- 0.55
  } else {
    flex <- 0.15
  }

  # Primary-role alignment bonus
  primary <- as.character(player_row$tactical_role_primary[[1]] %||% "")
  role_align <- if (identical(primary, role_id)) 1.0 else 0.75

  # Blend influence note: higher minutes_share → more current-season weight already in blended metrics
  ytd_infl <- clip01(as.numeric(player_row$minutes_share[[1]] %||% 0.3))

  weight <- (
    0.28 * role_fit +
      0.28 * proj +
      0.14 * sample_rel +
      0.12 * availability +
      0.10 * minutes_capacity +
      0.08 * flex
  ) * role_align

  # Slightly boost players with more current-season signal in the blend
  weight <- weight * (0.9 + 0.2 * ytd_infl)
  list(
    weight = as.numeric(weight),
    role_fit = role_fit * 100,
    proj = proj * 100,
    sample_rel = sample_rel,
    availability = availability,
    minutes_capacity = minutes_capacity,
    flex = flex,
    ytd_influence = ytd_infl
  )
}

#' Candidates for a club × position × role slot (includes flexible contributors).
#' Membership comes from current 2026 MLS roster (salary guide + active minutes).
slot_candidates <- function(players_df, team_ids, role_id, position_group, club_id = NULL) {
  ensure_packages("dplyr")
  flex_set <- POSITION_FLEX[[position_group]] %||% position_group

  member_ids <- NULL
  if (!is.null(club_id)) {
    rost <- club_roster_players(players_df, club_id, active_only = TRUE)
    if (nrow(rost)) member_ids <- unique(rost$asa_player_id)
  }

  out <- players_df |>
    dplyr::filter(
      .data$league_id == "mls",
      .data$role_id == .env$role_id,
      .data$position_group %in% flex_set
    )

  if (!is.null(member_ids)) {
    out <- dplyr::filter(out, .data$asa_player_id %in% member_ids)
  } else if (length(team_ids)) {
    out <- dplyr::filter(out, .data$team_id %in% team_ids)
  }

  out |>
    dplyr::group_by(.data$asa_player_id) |>
    dplyr::slice_max(order_by = .data$score_role_fit + 0.01 * .data$minutes, n = 1, with_ties = FALSE) |>
    dplyr::ungroup()
}

tactical_coverage_raw <- function(cands, club, role_id) {
  if (!nrow(cands)) return(35)
  # Weight club style dimensions against role-relevant percentiles among candidates
  press <- mean(cands$pct_proj_press, na.rm = TRUE)
  prog <- mean(cands$pct_prog_pass, na.rm = TRUE)
  carry <- mean(cands$pct_prog_carry, na.rm = TRUE)
  def <- mean(cands$pct_tackles + cands$pct_intercept, na.rm = TRUE) / 2
  poss <- mean(cands$pct_pass, na.rm = TRUE)

  target <- (
    (club$pressing_weight %||% 0.5) * press +
      (club$progression_weight %||% 0.5) * prog +
      (club$transition_weight %||% 0.5) * carry +
      (club$defensive_weight %||% 0.5) * def +
      (club$possession_weight %||% 0.5) * poss
  ) / max(
    (club$pressing_weight %||% 0.5) + (club$progression_weight %||% 0.5) +
      (club$transition_weight %||% 0.5) + (club$defensive_weight %||% 0.5) +
      (club$possession_weight %||% 0.5),
    1e-6
  )
  if (!is.finite(target)) 40 else as.numeric(target)
}

#' Raw (pre-percentile) metrics for one club × position × role.
compute_slot_raw_metrics <- function(players_df, club, role_id, position_group,
                                     team_map = NULL, cfg = NULL) {
  team_map <- team_map %||% load_club_team_map()
  team_ids <- asa_team_ids_for_club(club$club_id, team_map)
  cands <- slot_candidates(
    players_df, team_ids, role_id, position_group, club_id = club$club_id
  )

  empty <- list(
    n_candidates = 0L,
    starter_quality = 0,
    effective_depth = 0,
    succession_safety = 50,
    availability_continuity = 40,
    tactical_coverage = 35,
    financial_efficiency = 50,
    minutes_concentration = 1,
    best_name = NA_character_,
    backup_name = NA_character_,
    best_age = NA_real_,
    best_minutes = NA_real_,
    backup_minutes = NA_real_,
    mean_age_top = NA_real_,
    salary_coverage = 0,
    metric_coverage = 0,
    position_certainty = 0,
    mean_sample_rel = 0,
    mean_ytd_influence = 0.3
  )
  if (!length(team_ids) || !nrow(cands)) return(empty)

  weights <- vapply(seq_len(nrow(cands)), function(i) {
    player_role_depth_weight(cands[i, , drop = FALSE], role_id, position_group, cfg)$weight
  }, numeric(1))

  ord <- order(weights, decreasing = TRUE)
  cands <- cands[ord, , drop = FALSE]
  weights <- weights[ord]

  best <- cands[1, , drop = FALSE]
  backup <- if (nrow(cands) >= 2) cands[2, , drop = FALSE] else NULL

  starter_quality <- as.numeric(best$score_projected_mls[[1]]) *
    (0.6 + 0.4 * as.numeric(best$score_role_fit[[1]]) / 100)

  # Effective depth: diminishing returns on stacked contributors
  decay <- 1 / (seq_along(weights)^0.65)
  effective_depth <- sum(weights * decay) * 100

  # Minutes concentration among players with any weight
  mins <- as.numeric(cands$minutes)
  mins_pos <- mins[mins > 0]
  minutes_concentration <- if (length(mins_pos)) max(mins_pos) / sum(mins_pos) else 1

  # Succession safety (higher = safer): younger depth with development score
  ages <- as.numeric(cands$age)
  young_depth <- sum(weights[ages <= 24], na.rm = TRUE)
  starter_age <- as.numeric(best$age[[1]])
  age_pressure <- clip01((starter_age - 27) / 8) # rises after ~27
  succ_safety <- clip100(
    35 + 40 * clip01(young_depth / max(sum(weights), 1e-6)) +
      25 * (1 - age_pressure) +
      0.15 * mean(as.numeric(cands$score_development), na.rm = TRUE)
  )

  # Availability / continuity (higher = healthier continuity)
  mean_risk <- mean(as.numeric(cands$score_risk), na.rm = TRUE)
  avail_cont <- clip100(
    70 - 25 * minutes_concentration - 0.25 * mean_risk +
      15 * clip01(sum(weights) / 1.5)
  )

  tactical <- tactical_coverage_raw(cands, club, role_id)

  # Financial efficiency of the slot (higher = more efficient unit)
  # Not a sporting need by itself
  cost <- mean(as.numeric(cands$cost_tier), na.rm = TRUE)
  fin_eff <- clip100(
    mean(as.numeric(cands$score_financial_value), na.rm = TRUE) * 0.7 +
      (6 - cost) * 8
  )

  sal_cov <- mean(is.finite(as.numeric(cands$salary)))
  metric_cov <- mean(is.finite(as.numeric(cands$score_role_fit)) &
                       is.finite(as.numeric(cands$score_projected_mls)))
  pos_cert <- mean(cands$position_group == position_group)
  sample_bits <- lapply(seq_len(min(5L, nrow(cands))), function(i) {
    player_role_depth_weight(cands[i, , drop = FALSE], role_id, position_group, cfg)
  })
  mean_sample <- mean(vapply(sample_bits, `[[`, 0, "sample_rel"))
  mean_ytd <- mean(vapply(sample_bits, `[[`, 0, "ytd_influence"))

  list(
    n_candidates = nrow(cands),
    starter_quality = as.numeric(starter_quality),
    effective_depth = as.numeric(effective_depth),
    succession_safety = as.numeric(succ_safety),
    availability_continuity = as.numeric(avail_cont),
    tactical_coverage = as.numeric(tactical),
    financial_efficiency = as.numeric(fin_eff),
    minutes_concentration = as.numeric(minutes_concentration),
    best_name = as.character(best$display_name[[1]]),
    backup_name = if (!is.null(backup)) as.character(backup$display_name[[1]]) else "—",
    best_age = as.numeric(best$age[[1]]),
    best_minutes = as.numeric(best$minutes[[1]]),
    backup_minutes = if (!is.null(backup)) as.numeric(backup$minutes[[1]]) else 0,
    mean_age_top = mean(ages[seq_len(min(2L, length(ages)))], na.rm = TRUE),
    salary_coverage = sal_cov,
    metric_coverage = metric_cov,
    position_certainty = pos_cert,
    mean_sample_rel = mean_sample,
    mean_ytd_influence = mean_ytd
  )
}

percentile_among <- function(value, distribution, higher_better = TRUE) {
  d <- distribution[is.finite(distribution)]
  if (!length(d) || !is.finite(value)) return(50)
  # Empirical percentile of `value` within league distribution
  p <- mean(d <= value) * 100
  if (!higher_better) p <- 100 - p
  clip100(p)
}

gap_from_percentile <- function(pct_good) {
  # Low league standing → high gap / need
  clip100(100 - pct_good)
}

priority_from_need <- function(need_score) {
  dplyr::case_when(
    need_score >= 75 ~ "Urgent",
    need_score >= 60 ~ "High",
    need_score >= 40 ~ "Moderate",
    TRUE ~ "Monitor"
  )
}

confidence_label <- function(score) {
  dplyr::case_when(
    score >= 70 ~ "High",
    score >= 45 ~ "Medium",
    TRUE ~ "Low"
  )
}

classify_need_type <- function(comp, sporting_gap) {
  # comp: named gaps; sporting_gap = max of quality/depth/succession/availability/tactical
  if (!sporting_gap && (comp$financial_efficiency_opportunity %||% 0) >= 55) {
    return("Efficiency opportunity")
  }
  ord <- sort(unlist(comp[c(
    "starter_quality_gap", "effective_depth_gap", "succession_risk",
    "availability_risk", "tactical_coverage_gap"
  )]), decreasing = TRUE)
  top <- names(ord)[1]
  dplyr::case_when(
    identical(top, "starter_quality_gap") ~ "Starter-quality gap",
    identical(top, "effective_depth_gap") ~ "Effective-depth gap",
    identical(top, "succession_risk") ~ "Succession risk",
    identical(top, "availability_risk") ~ "Availability / continuity risk",
    identical(top, "tactical_coverage_gap") ~ "Tactical-coverage gap",
    TRUE ~ "Composite roster need"
  )
}

suggested_recruitment_profile <- function(role_id, position_group, club, need_type, comp, slot) {
  role_label <- gsub("_", " ", role_id)
  # Age band from succession vs immediate priority
  if (identical(need_type, "Succession risk") || (comp$succession_risk %||% 0) >= 60) {
    age_band <- "18–24"
    priority <- "Development / succession"
  } else if ((club$immediate_impact_priority %||% 0.5) >= 0.65 ||
             identical(need_type, "Starter-quality gap")) {
    age_band <- "23–29"
    priority <- "Immediate contributor"
  } else {
    age_band <- "20–27"
    priority <- "Balanced (contribute now, grow later)"
  }

  cost <- if ((comp$financial_efficiency_opportunity %||% 0) >= 55) {
    "prefer cost tiers 1–3 (value)"
  } else if (identical(need_type, "Starter-quality gap")) {
    sprintf("cost tier matching club budget (%s)", club$budget_tier %||% "medium")
  } else {
    "cost tier 1–3 unless clear starter upgrade"
  }

  strengths <- dplyr::case_when(
    identical(role_id, "pressing_striker") ~
      "pressing intensity, npxG volume, transition runs",
    identical(role_id, "transition_winger") ~
      "progressive carries, chance creation in transition, wide threat",
    identical(role_id, "ball_winning_midfielder") ~
      "duels/interceptions, progressive passing after regain",
    identical(role_id, "progressive_center_back") ~
      "line-breaking passes/carries with defensive security",
    identical(role_id, "overlapping_fullback") ~
      "width, crosses/cutbacks, recovery defending",
    TRUE ~ "role-defining traits for the selected tactical role"
  )

  sprintf(
    paste0(
      "Search profile (public-data suggestion): %s / %s; age %s; %s; %s; ",
      "required strengths: %s. Validate with video, medical, contract, and tactical staff context — ",
      "do not treat as an order to replace a named incumbent."
    ),
    position_group, role_label, age_band, cost, priority, strengths
  )
}

evidence_summary <- function(slot, comp, pcts) {
  sprintf(
    paste0(
      "Best public-data option: %s (age %s, %.0f′). Backup: %s (%.0f′). ",
      "Starter-quality league pct %.0f; effective-depth pct %.0f; ",
      "minutes concentration %.0f%%; succession risk %.0f; ",
      "availability risk %.0f; tactical-coverage gap %.0f; ",
      "efficiency opportunity %.0f. ",
      "Uses blended 2026 YTD + 2025 prior metrics (2026 influence rises with current minutes)."
    ),
    slot$best_name %||% "none identified",
    if (is.finite(slot$best_age)) sprintf("%.0f", slot$best_age) else "?",
    slot$best_minutes %||% 0,
    slot$backup_name %||% "—",
    slot$backup_minutes %||% 0,
    pcts$starter_quality %||% 50,
    pcts$effective_depth %||% 50,
    100 * (slot$minutes_concentration %||% 0),
    comp$succession_risk,
    comp$availability_risk,
    comp$tactical_coverage_gap,
    comp$financial_efficiency_opportunity
  )
}

public_data_limitations <- function(conf_label) {
  paste0(
    "Public-data limitations: no medical/availability, contract status, option years, ",
    "or internal tactical roles. Position labels and minutes are ASA-derived estimates. ",
    "Salary from MLSPA/ASA guide may lag. Confidence: ", conf_label, ". ",
    "Staff, video, medical, and contract context required before acting."
  )
}

slot_confidence <- function(slot) {
  # Minutes / sample, metric coverage, position certainty, salary quality, model reliability
  mins_score <- clip100(100 * (slot$mean_sample_rel %||% 0))
  metric_score <- clip100(100 * (slot$metric_coverage %||% 0))
  pos_score <- clip100(100 * (slot$position_certainty %||% 0))
  sal_score <- clip100(100 * (slot$salary_coverage %||% 0))
  # Availability-data quality is weak in public data — cap contribution
  avail_data_quality <- 35
  model_rel <- clip100(55 + 25 * (slot$mean_ytd_influence %||% 0.3) +
                         10 * clip01((slot$n_candidates %||% 0) / 4))
  clip100(
    0.25 * mins_score +
      0.20 * metric_score +
      0.15 * pos_score +
      0.10 * avail_data_quality +
      0.15 * sal_score +
      0.15 * model_rel
  )
}

#' Build league distributions of raw slot metrics for percentile benchmarking.
build_league_slot_benchmarks <- function(players_df, clubs_list, team_map = NULL,
                                         cfg = NULL, roles = NULL) {
  ensure_packages("dplyr")
  team_map <- team_map %||% load_club_team_map()
  roles <- roles %||% names(ROLE_POSITION_MAP)
  rows <- list()
  for (club in clubs_list) {
    for (role_id in roles) {
      pos <- ROLE_POSITION_MAP[[role_id]]
      raw <- compute_slot_raw_metrics(players_df, club, role_id, pos, team_map, cfg)
      rows[[length(rows) + 1]] <- tibble::tibble(
        club_id = club$club_id,
        position_group = pos,
        tactical_role = role_id,
        starter_quality = raw$starter_quality,
        effective_depth = raw$effective_depth,
        succession_safety = raw$succession_safety,
        availability_continuity = raw$availability_continuity,
        tactical_coverage = raw$tactical_coverage,
        financial_efficiency = raw$financial_efficiency
      )
    }
  }
  dplyr::bind_rows(rows)
}

#' Evaluate needs for one club across all tactical roles (club × position × role).
analyze_club_role_needs <- function(players_df, club, clubs_list = NULL,
                                    team_map = NULL, roles_yaml = NULL,
                                    cfg = NULL, league_bench = NULL) {
  ensure_packages(c("dplyr", "tibble"))
  team_map <- team_map %||% load_club_team_map()
  cfg <- cfg %||% tryCatch(load_config(), error = function(e) NULL)
  weights <- need_weights_from_cfg(cfg)
  roles <- names(ROLE_POSITION_MAP)

  if (is.null(clubs_list)) {
    clubs_yaml <- load_yaml("config/club_profiles.yml")
    clubs_list <- clubs_yaml$clubs
  }

  if (is.null(league_bench)) {
    league_bench <- build_league_slot_benchmarks(players_df, clubs_list, team_map, cfg, roles)
  }

  roster <- club_roster_players(players_df, club$club_id, team_map)
  if (!nrow(roster)) {
    return(tibble::tibble(
      club_id = club$club_id,
      club_name = club$club_name %||% club$club_id,
      position_group = NA_character_,
      tactical_role = NA_character_,
      need_score = 90,
      confidence = "Low",
      confidence_score = 20,
      need_type = "Missing roster link",
      priority = "Urgent",
      current_best = NA_character_,
      best_backup = NA_character_,
      starter_quality_percentile = NA_real_,
      effective_depth_percentile = NA_real_,
      minutes_concentration = NA_real_,
      succession_risk = NA_real_,
      starter_quality_gap = NA_real_,
      effective_depth_gap = NA_real_,
      availability_risk = NA_real_,
      tactical_coverage_gap = NA_real_,
      financial_efficiency_opportunity = NA_real_,
      evidence_summary = "No MLS roster rows matched this club's ASA team mapping.",
      suggested_recruitment_profile = "Re-check club↔ASA team mapping before recruiting.",
      public_data_limitations = public_data_limitations("Low"),
      suggested_role = NA_character_,
      source_label = "Public-data-based roster-planning signal — not an official club priority",
      override_allowed = TRUE
    ))
  }

  out <- lapply(roles, function(role_id) {
    pos <- ROLE_POSITION_MAP[[role_id]]
    slot <- compute_slot_raw_metrics(players_df, club, role_id, pos, team_map, cfg)
    dist <- dplyr::filter(league_bench, .data$tactical_role == .env$role_id)

    pct_starter <- percentile_among(slot$starter_quality, dist$starter_quality, TRUE)
    pct_depth <- percentile_among(slot$effective_depth, dist$effective_depth, TRUE)
    pct_succ_safe <- percentile_among(slot$succession_safety, dist$succession_safety, TRUE)
    pct_avail <- percentile_among(slot$availability_continuity, dist$availability_continuity, TRUE)
    pct_tact <- percentile_among(slot$tactical_coverage, dist$tactical_coverage, TRUE)
    pct_fin <- percentile_among(slot$financial_efficiency, dist$financial_efficiency, TRUE)

    comp <- list(
      starter_quality_gap = gap_from_percentile(pct_starter),
      effective_depth_gap = gap_from_percentile(pct_depth),
      succession_risk = gap_from_percentile(pct_succ_safe),
      availability_risk = gap_from_percentile(pct_avail),
      tactical_coverage_gap = gap_from_percentile(pct_tact),
      financial_efficiency_opportunity = gap_from_percentile(pct_fin)
    )

    need_score <- clip100(
      weights$starter_quality_gap * comp$starter_quality_gap +
        weights$effective_depth_gap * comp$effective_depth_gap +
        weights$succession_risk * comp$succession_risk +
        weights$availability_risk * comp$availability_risk +
        weights$tactical_coverage_gap * comp$tactical_coverage_gap +
        weights$financial_efficiency_opportunity * comp$financial_efficiency_opportunity
    )

    sporting_max <- max(
      comp$starter_quality_gap, comp$effective_depth_gap, comp$succession_risk,
      comp$availability_risk, comp$tactical_coverage_gap,
      na.rm = TRUE
    )
    sporting_gap <- is.finite(sporting_max) && sporting_max >= 45
    need_type <- classify_need_type(comp, sporting_gap)

    # If only efficiency is elevated, keep need score but label as efficiency opportunity
    if (!sporting_gap && comp$financial_efficiency_opportunity >= 50) {
      need_type <- "Efficiency opportunity"
    }

    conf_score <- slot_confidence(slot)
    conf_lab <- confidence_label(conf_score)
    pcts <- list(starter_quality = pct_starter, effective_depth = pct_depth)

    tibble::tibble(
      club_id = club$club_id,
      club_name = club$club_name %||% club$club_id,
      position_group = pos,
      tactical_role = role_id,
      need_score = round(need_score, 1),
      confidence = conf_lab,
      confidence_score = round(conf_score, 1),
      need_type = need_type,
      priority = priority_from_need(need_score),
      current_best = slot$best_name,
      best_backup = slot$backup_name,
      starter_quality_percentile = round(pct_starter, 1),
      effective_depth_percentile = round(pct_depth, 1),
      minutes_concentration = round(100 * slot$minutes_concentration, 1),
      succession_risk = round(comp$succession_risk, 1),
      starter_quality_gap = round(comp$starter_quality_gap, 1),
      effective_depth_gap = round(comp$effective_depth_gap, 1),
      availability_risk = round(comp$availability_risk, 1),
      tactical_coverage_gap = round(comp$tactical_coverage_gap, 1),
      financial_efficiency_opportunity = round(comp$financial_efficiency_opportunity, 1),
      evidence_summary = evidence_summary(slot, comp, pcts),
      suggested_recruitment_profile = suggested_recruitment_profile(
        role_id, pos, club, need_type, comp, slot
      ),
      public_data_limitations = public_data_limitations(conf_lab),
      suggested_role = role_id,
      source_label = "Public-data-based roster-planning signal — not an official club priority",
      override_allowed = TRUE
    )
  })

  dplyr::bind_rows(out) |>
    dplyr::arrange(dplyr::desc(.data$need_score), .data$tactical_role)
}

#' Backward-compatible wrapper used by the Shiny app.
analyze_roster_gaps <- function(players_df, club, team_map = NULL, roles_yaml = NULL,
                                cfg = NULL, clubs_list = NULL) {
  analyze_club_role_needs(
    players_df = players_df,
    club = club,
    clubs_list = clubs_list,
    team_map = team_map,
    roles_yaml = roles_yaml,
    cfg = cfg
  )
}

roster_summary_table <- function(roster_df) {
  ensure_packages("dplyr")
  if (!nrow(roster_df)) {
    return(tibble::tibble(
      Player = character(), Position = character(), Role = character(),
      Age = numeric(), `Minutes 2026` = numeric(),
      `Salary (USD)` = character(), `Cost tier` = integer(),
      `Estimated Near-Term Contribution` = character(), `Tactical Role Fit` = character(),
      `Development Upside` = character(), `Model Uncertainty` = character()
    ))
  }
  mins <- if ("minutes_2026" %in% names(roster_df)) {
    dplyr::coalesce(roster_df$minutes_2026, roster_df$minutes)
  } else {
    roster_df$minutes
  }
  sal <- if ("salary_usd" %in% names(roster_df)) {
    dplyr::coalesce(roster_df$salary_usd, roster_df$salary)
  } else {
    roster_df$salary
  }
  roster_df |>
    dplyr::transmute(
      Player = .data$display_name,
      Position = .data$position_group,
      Role = .data$tactical_role_primary,
      Age = round(.data$age, 1),
      `Minutes 2026` = round(mins),
      `Salary (USD)` = fmt_usd(sal, "USD"),
      `Cost tier` = as.integer(.data$cost_tier),
      `Estimated Near-Term Contribution` = fmt_score_with_rank(.data$score_projected_mls),
      `Tactical Role Fit` = fmt_score_with_rank(.data$score_role_fit),
      `Development Upside` = fmt_score_with_rank(.data$score_development),
      `Model Uncertainty` = fmt_score_with_rank(.data$score_risk, higher_better = FALSE)
    )
}

`%||%` <- function(a, b) if (!is.null(a)) a else b
