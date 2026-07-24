-- MLS Recruitment Intelligence
-- SQLite-first schema (PostgreSQL-compatible types where practical)
-- Apply with: sqlite3 database/mls_recruitment.sqlite < database/schema.sql

PRAGMA foreign_keys = ON;

-- ---------------------------------------------------------------------------
-- Reference / identity
-- ---------------------------------------------------------------------------

CREATE TABLE IF NOT EXISTS leagues (
  league_id        TEXT PRIMARY KEY,
  league_name      TEXT NOT NULL,
  federation       TEXT,
  tier_code        TEXT,
  country          TEXT,
  is_mls_recruitment_market INTEGER NOT NULL DEFAULT 1,
  created_at       TEXT NOT NULL DEFAULT (datetime('now'))
);

CREATE TABLE IF NOT EXISTS seasons (
  season_id        INTEGER PRIMARY KEY AUTOINCREMENT,
  season_year      INTEGER NOT NULL,
  label            TEXT,
  UNIQUE (season_year)
);

CREATE TABLE IF NOT EXISTS competitions (
  competition_id   TEXT PRIMARY KEY,
  league_id        TEXT NOT NULL REFERENCES leagues(league_id),
  season_year      INTEGER NOT NULL,
  competition_name TEXT NOT NULL,
  UNIQUE (league_id, season_year)
);

CREATE TABLE IF NOT EXISTS teams (
  team_id          TEXT PRIMARY KEY,
  team_name        TEXT NOT NULL,
  team_short       TEXT,
  league_id        TEXT REFERENCES leagues(league_id),
  conference       TEXT,
  is_mls_club      INTEGER NOT NULL DEFAULT 0,
  asa_team_id      TEXT,
  created_at       TEXT NOT NULL DEFAULT (datetime('now'))
);

CREATE INDEX IF NOT EXISTS idx_teams_league ON teams(league_id);

CREATE TABLE IF NOT EXISTS players (
  player_id        TEXT PRIMARY KEY,
  display_name     TEXT NOT NULL,
  normalized_name  TEXT NOT NULL,
  birth_date       TEXT,
  nationality      TEXT,
  primary_position TEXT,
  is_domestic_player INTEGER, -- relative to MLS roster rules proxy (US/CAN/GC)
  asa_player_id    TEXT,
  fbref_player_id  TEXT,
  height_cm        REAL,
  preferred_foot   TEXT,
  created_at       TEXT NOT NULL DEFAULT (datetime('now')),
  updated_at       TEXT NOT NULL DEFAULT (datetime('now'))
);

CREATE INDEX IF NOT EXISTS idx_players_normalized_name ON players(normalized_name);
CREATE INDEX IF NOT EXISTS idx_players_asa ON players(asa_player_id);

CREATE TABLE IF NOT EXISTS player_positions (
  player_id        TEXT NOT NULL REFERENCES players(player_id),
  season_year      INTEGER NOT NULL,
  position_group   TEXT NOT NULL, -- FW W CM FB CB GK
  position_detail  TEXT,
  is_primary       INTEGER NOT NULL DEFAULT 1,
  PRIMARY KEY (player_id, season_year, position_group)
);

-- ---------------------------------------------------------------------------
-- Performance
-- ---------------------------------------------------------------------------

CREATE TABLE IF NOT EXISTS player_season_stats (
  stat_id          INTEGER PRIMARY KEY AUTOINCREMENT,
  player_id        TEXT NOT NULL REFERENCES players(player_id),
  team_id          TEXT REFERENCES teams(team_id),
  league_id        TEXT NOT NULL REFERENCES leagues(league_id),
  season_year      INTEGER NOT NULL,
  minutes          REAL,
  games            INTEGER,
  npxg             REAL,
  xa               REAL,
  shots            REAL,
  goals            REAL,
  assists          REAL,
  xpass_diff       REAL,
  goals_added      REAL,
  goals_added_dribbling REAL,
  goals_added_passing   REAL,
  goals_added_receiving REAL,
  goals_added_shooting  REAL,
  goals_added_defending REAL,
  tackles          REAL,
  interceptions    REAL,
  pressures_proxy  REAL,
  progressive_passes_proxy REAL,
  progressive_carries_proxy REAL,
  aerial_wins      REAL,
  aerial_duels     REAL,
  raw_payload_json TEXT,
  source           TEXT,
  retrieved_at     TEXT,
  UNIQUE (player_id, team_id, league_id, season_year)
);

CREATE INDEX IF NOT EXISTS idx_pss_player_season ON player_season_stats(player_id, season_year);
CREATE INDEX IF NOT EXISTS idx_pss_league_season ON player_season_stats(league_id, season_year);

CREATE TABLE IF NOT EXISTS player_match_stats (
  match_stat_id    INTEGER PRIMARY KEY AUTOINCREMENT,
  player_id        TEXT NOT NULL REFERENCES players(player_id),
  team_id          TEXT REFERENCES teams(team_id),
  league_id        TEXT NOT NULL REFERENCES leagues(league_id),
  season_year      INTEGER NOT NULL,
  game_id          TEXT,
  match_date       TEXT,
  minutes          REAL,
  npxg             REAL,
  xa               REAL,
  goals_added      REAL,
  source           TEXT,
  retrieved_at     TEXT
);

CREATE INDEX IF NOT EXISTS idx_pms_player ON player_match_stats(player_id, season_year);

-- ---------------------------------------------------------------------------
-- Roster / salary / contracts / transfers
-- ---------------------------------------------------------------------------

CREATE TABLE IF NOT EXISTS roster_records (
  roster_id        INTEGER PRIMARY KEY AUTOINCREMENT,
  player_id        TEXT NOT NULL REFERENCES players(player_id),
  team_id          TEXT NOT NULL REFERENCES teams(team_id),
  season_year      INTEGER NOT NULL,
  roster_status    TEXT, -- senior, supplemental, inactive, loan, etc.
  international_slot INTEGER,
  start_date       TEXT,
  end_date         TEXT,
  source           TEXT,
  retrieved_at     TEXT
);

CREATE TABLE IF NOT EXISTS salary_records (
  salary_id        INTEGER PRIMARY KEY AUTOINCREMENT,
  player_id        TEXT NOT NULL REFERENCES players(player_id),
  team_id          TEXT REFERENCES teams(team_id),
  season_year      INTEGER NOT NULL,
  base_salary      REAL,
  guaranteed_comp  REAL,
  currency         TEXT DEFAULT 'USD',
  as_of_date       TEXT,
  source           TEXT,
  retrieved_at     TEXT,
  UNIQUE (player_id, season_year, as_of_date, source)
);

CREATE INDEX IF NOT EXISTS idx_salary_player ON salary_records(player_id, season_year);

CREATE TABLE IF NOT EXISTS contract_records (
  contract_id      INTEGER PRIMARY KEY AUTOINCREMENT,
  player_id        TEXT NOT NULL REFERENCES players(player_id),
  team_id          TEXT REFERENCES teams(team_id),
  season_year      INTEGER,
  contract_end_year INTEGER,
  is_expiring      INTEGER,
  is_loan          INTEGER,
  notes            TEXT,
  confidence       REAL,
  source           TEXT,
  retrieved_at     TEXT
);

CREATE TABLE IF NOT EXISTS transfer_records (
  transfer_id      INTEGER PRIMARY KEY AUTOINCREMENT,
  player_id        TEXT NOT NULL REFERENCES players(player_id),
  from_team_id     TEXT,
  to_team_id       TEXT,
  from_league_id   TEXT,
  to_league_id     TEXT,
  transfer_date    TEXT,
  fee_usd          REAL,
  fee_tier         INTEGER,
  transfer_type    TEXT, -- transfer, loan, free, draft, trade
  source           TEXT,
  retrieved_at     TEXT
);

-- ---------------------------------------------------------------------------
-- Tactical roles & configuration persisted
-- ---------------------------------------------------------------------------

CREATE TABLE IF NOT EXISTS tactical_roles (
  role_id          TEXT PRIMARY KEY,
  display_name     TEXT NOT NULL,
  position_group   TEXT NOT NULL,
  description      TEXT,
  is_mvp           INTEGER NOT NULL DEFAULT 0
);

CREATE TABLE IF NOT EXISTS role_metric_weights (
  role_id          TEXT NOT NULL REFERENCES tactical_roles(role_id),
  metric_name      TEXT NOT NULL,
  weight           REAL NOT NULL,
  PRIMARY KEY (role_id, metric_name)
);

CREATE TABLE IF NOT EXISTS league_translation_factors (
  league_id        TEXT NOT NULL REFERENCES leagues(league_id),
  metric_family    TEXT NOT NULL, -- attack, creation, defense
  translation_factor REAL NOT NULL,
  uncertainty      REAL NOT NULL,
  model_version    TEXT NOT NULL,
  notes            TEXT,
  PRIMARY KEY (league_id, metric_family, model_version)
);

-- ---------------------------------------------------------------------------
-- Club profiles & needs
-- ---------------------------------------------------------------------------

CREATE TABLE IF NOT EXISTS club_profiles (
  club_id          TEXT PRIMARY KEY,
  club_name        TEXT NOT NULL,
  conference       TEXT,
  tactical_archetype TEXT,
  recruitment_strategy TEXT,
  budget_tier      TEXT,
  average_squad_age REAL,
  domestic_player_priority REAL,
  development_priority REAL,
  immediate_impact_priority REAL,
  pressing_weight  REAL,
  possession_weight REAL,
  transition_weight REAL,
  progression_weight REAL,
  defensive_weight REAL,
  financial_value_weight REAL,
  profile_label    TEXT DEFAULT 'Public-data-based estimated club profile',
  season_year      INTEGER,
  updated_at       TEXT NOT NULL DEFAULT (datetime('now'))
);

CREATE TABLE IF NOT EXISTS club_needs (
  need_id          INTEGER PRIMARY KEY AUTOINCREMENT,
  club_id          TEXT NOT NULL REFERENCES club_profiles(club_id),
  season_year      INTEGER NOT NULL,
  position         TEXT NOT NULL,
  tactical_role    TEXT REFERENCES tactical_roles(role_id),
  need_priority    INTEGER NOT NULL DEFAULT 3,
  age_preference   TEXT,
  maximum_cost_tier INTEGER,
  starter_or_depth TEXT,
  notes            TEXT
);

CREATE INDEX IF NOT EXISTS idx_club_needs_club ON club_needs(club_id, season_year);

-- ---------------------------------------------------------------------------
-- Models & rankings
-- ---------------------------------------------------------------------------

CREATE TABLE IF NOT EXISTS model_predictions (
  prediction_id    INTEGER PRIMARY KEY AUTOINCREMENT,
  player_id        TEXT NOT NULL REFERENCES players(player_id),
  season_year      INTEGER NOT NULL,
  role_id          TEXT NOT NULL REFERENCES tactical_roles(role_id),
  model_version    TEXT NOT NULL,
  proj_npxg_p90    REAL,
  proj_xa_p90      REAL,
  proj_gplus_p90   REAL,
  score_projected_mls REAL,
  score_role_fit   REAL,
  score_feasibility REAL,
  score_development REAL,
  score_financial_value REAL,
  score_risk       REAL,
  confidence       TEXT,
  data_quality     REAL,
  strengths_json   TEXT,
  risks_json       TEXT,
  video_questions_json TEXT,
  created_at       TEXT NOT NULL DEFAULT (datetime('now')),
  UNIQUE (player_id, season_year, role_id, model_version)
);

CREATE TABLE IF NOT EXISTS scouting_rankings (
  ranking_id       INTEGER PRIMARY KEY AUTOINCREMENT,
  run_id           TEXT NOT NULL,
  club_id          TEXT REFERENCES club_profiles(club_id),
  role_id          TEXT NOT NULL REFERENCES tactical_roles(role_id),
  player_id        TEXT NOT NULL REFERENCES players(player_id),
  season_year      INTEGER NOT NULL,
  rank             INTEGER,
  score_overall    REAL,
  score_club_fit   REAL,
  score_projected_mls REAL,
  score_role_fit   REAL,
  score_feasibility REAL,
  score_development REAL,
  score_financial_value REAL,
  score_risk       REAL,
  recommendation   TEXT,
  explanation      TEXT,
  weights_json     TEXT,
  created_at       TEXT NOT NULL DEFAULT (datetime('now'))
);

CREATE INDEX IF NOT EXISTS idx_rankings_run ON scouting_rankings(run_id, club_id, role_id);

-- ---------------------------------------------------------------------------
-- Provenance
-- ---------------------------------------------------------------------------

CREATE TABLE IF NOT EXISTS data_sources (
  source_id        INTEGER PRIMARY KEY AUTOINCREMENT,
  source_name      TEXT NOT NULL,
  source_url       TEXT,
  entity_type      TEXT,
  league_id        TEXT,
  season_year      INTEGER,
  retrieved_at     TEXT NOT NULL,
  record_count     INTEGER,
  checksum         TEXT,
  notes            TEXT
);

CREATE TABLE IF NOT EXISTS pipeline_runs (
  run_id           TEXT PRIMARY KEY,
  mode             TEXT NOT NULL, -- demo | live
  started_at       TEXT NOT NULL,
  finished_at      TEXT,
  config_snapshot  TEXT,
  status           TEXT,
  notes            TEXT
);
