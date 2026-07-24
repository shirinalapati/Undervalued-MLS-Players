# American Soccer Analysis collector (live mode)
# Uses itscalledsoccer — public API, no custom HTML scraping.

cache_is_fresh <- function(path, cache_hours, force = FALSE) {
  if (isTRUE(force)) return(FALSE)
  if (!file.exists(path)) return(FALSE)
  hours <- as.numeric(cache_hours %||% 12)
  if (!is.finite(hours) || hours <= 0) return(FALSE)
  age_h <- as.numeric(difftime(Sys.time(), file.info(path)$mtime, units = "hours"))
  is.finite(age_h) && age_h < hours
}

collect_asa_league <- function(asa, league, seasons, cache_dir, pause_seconds = 1,
                               cache_hours = 12, force = FALSE) {
  ensure_packages(c("dplyr", "tidyr", "readr", "jsonlite"))
  dir_create_safe(cache_dir)

  out <- list()
  for (season in seasons) {
    key <- paste(league, season, sep = "_")
    cache_file <- file.path(cache_dir, paste0("asa_", key, ".rds"))

    if (cache_is_fresh(cache_file, cache_hours, force = force)) {
      write_log("Cache hit (fresh < ", cache_hours, "h): ", cache_file)
      out[[key]] <- readRDS(cache_file)
      next
    }
    if (file.exists(cache_file)) {
      write_log("Cache stale or force-refresh — re-fetching: ", cache_file)
    }

    write_log("Fetching ASA ", league, " ", season)
    Sys.sleep(pause_seconds)

    players <- tryCatch(
      asa$get_players(leagues = league),
      error = function(e) {
        write_log("get_players failed: ", e$message)
        NULL
      }
    )

    teams <- tryCatch(
      asa$get_teams(leagues = league),
      error = function(e) NULL
    )

    xg <- tryCatch(
      asa$get_player_xgoals(
        leagues = league,
        season_name = season,
        split_by_seasons = TRUE
      ),
      error = function(e) {
        write_log("xgoals failed: ", e$message)
        NULL
      }
    )

    Sys.sleep(pause_seconds)
    xpass <- tryCatch(
      asa$get_player_xpass(
        leagues = league,
        season_name = season,
        split_by_seasons = TRUE
      ),
      error = function(e) NULL
    )

    Sys.sleep(pause_seconds)
    gplus <- tryCatch(
      asa$get_player_goals_added(
        leagues = league,
        season_name = season,
        split_by_seasons = TRUE
      ),
      error = function(e) NULL
    )

    salaries <- NULL
    # Prefer product salary season (2026 MLSPA guide via ASA); also keep year-matched pulls
    if (identical(league, "mls")) {
      Sys.sleep(pause_seconds)
      sal_season <- season
      salaries <- tryCatch(
        asa$get_player_salaries(leagues = league, season_name = sal_season),
        error = function(e) NULL
      )
    }

    payload <- list(
      league = league,
      season = season,
      retrieved_at = as.character(Sys.time()),
      players = players,
      teams = teams,
      xgoals = xg,
      xpass = xpass,
      goals_added = gplus,
      salaries = salaries
    )
    saveRDS(payload, cache_file)
    out[[key]] <- payload
  }
  out
}

collect_live_asa <- function(cfg, force = FALSE) {
  ensure_packages("itscalledsoccer")
  asa <- itscalledsoccer::AmericanSoccerAnalysis$new()
  leagues <- cfg$acquisition$asa$leagues
  seasons <- cfg$acquisition$asa$seasons
  cache_dir <- file.path(cfg$paths$raw, "cache")
  pause <- cfg$acquisition$asa$pause_seconds %||% 1
  cache_hours <- cfg$acquisition$asa$cache_hours %||% 12
  force <- isTRUE(force) || identical(Sys.getenv("MLS_RI_FORCE_REFRESH"), "1")

  results <- list()
  for (lg in leagues) {
    results <- c(
      results,
      collect_asa_league(
        asa, lg, seasons, cache_dir, pause,
        cache_hours = cache_hours, force = force
      )
    )
  }

  # Explicit 2026 MLS salary-guide pull (MLSPA via ASA)
  salary_season <- cfg$acquisition$asa$salary_season %||% 2026
  sal_key <- paste0("mls_salaries_", salary_season)
  sal_cache <- file.path(cache_dir, paste0(sal_key, ".rds"))
  if (!cache_is_fresh(sal_cache, cache_hours, force = force)) {
    write_log("Fetching MLS salary guide season ", salary_season)
    Sys.sleep(pause)
    sal <- tryCatch(
      asa$get_player_salaries(leagues = "mls", season_name = salary_season),
      error = function(e) {
        write_log("Salary pull failed: ", e$message)
        NULL
      }
    )
    saveRDS(list(season = salary_season, retrieved_at = as.character(Sys.time()), salaries = sal), sal_cache)
  } else {
    write_log("Salary cache fresh: ", sal_cache)
  }

  saveRDS(results, file.path(cfg$paths$raw, "asa_collection.rds"))
  invisible(results)
}

`%||%` <- function(a, b) if (!is.null(a)) a else b
