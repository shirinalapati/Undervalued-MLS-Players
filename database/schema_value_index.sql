-- 2026 MLS Value Index schema (SQLite)
PRAGMA foreign_keys = ON;

CREATE TABLE IF NOT EXISTS seasons (
  season_year INTEGER PRIMARY KEY,
  label TEXT
);

CREATE TABLE IF NOT EXISTS teams (
  team_id TEXT PRIMARY KEY,
  team_name TEXT NOT NULL,
  team_short TEXT,
  league_id TEXT,
  is_mls_club INTEGER NOT NULL DEFAULT 0,
  asa_team_id TEXT
);

CREATE TABLE IF NOT EXISTS players (
  player_id TEXT PRIMARY KEY,
  asa_player_id TEXT,
  display_name TEXT NOT NULL,
  normalized_name TEXT,
  birth_date TEXT,
  nationality TEXT,
  primary_position TEXT
);

CREATE TABLE IF NOT EXISTS player_positions (
  asa_player_id TEXT NOT NULL,
  season_year INTEGER NOT NULL,
  position_group TEXT NOT NULL,
  is_primary INTEGER NOT NULL DEFAULT 1,
  PRIMARY KEY (asa_player_id, season_year, position_group)
);

CREATE TABLE IF NOT EXISTS player_season_stats (
  asa_player_id TEXT NOT NULL,
  season_year INTEGER NOT NULL,
  team_id TEXT,
  minutes REAL,
  goals_added_p90 REAL,
  PRIMARY KEY (asa_player_id, season_year, team_id)
);

CREATE TABLE IF NOT EXISTS goals_added_components (
  asa_player_id TEXT NOT NULL,
  season_year INTEGER NOT NULL,
  shooting_p90 REAL,
  passing_p90 REAL,
  receiving_p90 REAL,
  dribbling_p90 REAL,
  interrupting_p90 REAL,
  fouling_p90 REAL,
  PRIMARY KEY (asa_player_id, season_year)
);

CREATE TABLE IF NOT EXISTS compensation_records (
  asa_player_id TEXT NOT NULL,
  season_year INTEGER NOT NULL,
  guaranteed_compensation REAL NOT NULL,
  source TEXT,
  as_of_date TEXT,
  PRIMARY KEY (asa_player_id, season_year)
);

CREATE TABLE IF NOT EXISTS player_value_scores (
  asa_player_id TEXT NOT NULL,
  evaluation_period TEXT NOT NULL,
  position_group TEXT NOT NULL,
  sporting_impact REAL,
  compensation_percentile REAL,
  value_surplus REAL,
  undervaluation_score REAL,
  metric_coverage REAL,
  data_confidence TEXT,
  model_version TEXT NOT NULL,
  data_version TEXT,
  calculation_timestamp TEXT,
  PRIMARY KEY (asa_player_id, evaluation_period, model_version)
);

CREATE INDEX IF NOT EXISTS idx_value_scores_pos ON player_value_scores(position_group, undervaluation_score);

CREATE TABLE IF NOT EXISTS data_sources (
  source_id TEXT PRIMARY KEY,
  source_name TEXT NOT NULL,
  cutoff_date TEXT
);

CREATE TABLE IF NOT EXISTS pipeline_runs (
  run_id INTEGER PRIMARY KEY AUTOINCREMENT,
  started_at TEXT,
  finished_at TEXT,
  model_version TEXT,
  status TEXT
);

CREATE TABLE IF NOT EXISTS model_runs (
  model_run_id INTEGER PRIMARY KEY AUTOINCREMENT,
  model_version TEXT NOT NULL,
  evaluation_period TEXT NOT NULL,
  created_at TEXT NOT NULL DEFAULT (datetime('now'))
);
